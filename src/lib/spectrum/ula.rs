
use crate::keyboard::Keyboard;
use std::{sync::mpsc::{channel, Sender, Receiver}, thread, time::Duration};

const SCREEN_WIDTH : usize =256;
const SCREEN_HEIGHT : usize = 192;
const SCREEN_WIDTH_ATTRS : usize = 32;
const ATTRS_OFFSET : usize = 0x1800;
const BUFFER_SIZE : usize = 0x1B00;
pub const SCREEN_SIZE_BYTES : usize = 4*SCREEN_WIDTH*SCREEN_HEIGHT;

static mut OUTPUT_RGBA: [u8; SCREEN_SIZE_BYTES] = [0; SCREEN_SIZE_BYTES];

static mut BORDER_COLOR: u32 = 0xCFCFCFFF;

pub struct Ula;

pub struct DataBus {
  pub value: u8
}

pub trait Runner: Send + Sync {
  type ReturnType: Send;
  fn run(&self) -> Option<Self::ReturnType>;
}

const COLOUR_PALETTE : [u32;0x10]= [
  0x000000FF,
  0x0100CEFF,
  0xCF0100FF,
  0xCF01CEFF,
  0x00CF15FF,
  0x01CFCFFF,
  0xCFCF15FF,
  0xCFCFCFFF,
  0x000000FF,
  0x0200FDFF,
  0xFF0201FF,
  0xFF02FDFF,
  0x00FF1CFF,
  0x02FFFFFF,
  0xFFFF1DFF,
  0xFFFFFFFF
];

impl Runner for DataBus {
  type ReturnType = u8;

  fn run(&self) -> Option<Self::ReturnType> {
    thread::sleep(Duration::from_millis(20));
    return Some(self.value);
  }
}

impl Ula {

  pub fn next_vblank<TIn: Runner>(f: &'static TIn) -> impl FnOnce()-> Option<TIn::ReturnType> {
    let (sender, receiver) = channel::<Option<TIn::ReturnType>>();   
    let hand = thread::spawn(move || {
        sender.send(f.run()).unwrap(); 
    });
    let f = move || -> Option<TIn::ReturnType> {
        let res = receiver.recv().unwrap();
        hand.join().unwrap();
        return res;
    };
    return f;
  }

  pub fn convert_screen_buffer_rgba(src_buffer : &[u8]) {
    unsafe {
      let mut l = 0;
      assert_eq!(src_buffer.len(), BUFFER_SIZE);
      for j in 0..SCREEN_HEIGHT {
        for i in 0..SCREEN_WIDTH {
          let k = (i % 8) as u8;
          let (m, n) = (i / 8, j / 8);
          let src_offset = (j<<5 & 0x1800) | (j << 8 & 0x0700) | (j<<2 & 0x00E0) |
                          (m & 0x001F);
          assert!(src_offset < SCREEN_WIDTH_ATTRS*SCREEN_HEIGHT);
          let src_attr_offset = ATTRS_OFFSET+n*SCREEN_WIDTH_ATTRS+m;
          assert!(src_attr_offset < BUFFER_SIZE);
          let bit = src_buffer[src_offset] >> (7-k) & 0x01 == 0x01;
          if bit {
            let palete_index = ((src_buffer[src_attr_offset] & 0x07) |
                                       ((src_buffer[src_attr_offset] & 0x40) >> 3)) as usize;
            OUTPUT_RGBA[l] = ((COLOUR_PALETTE[palete_index] >> 24) & 0xFF) as u8;
            OUTPUT_RGBA[l+1] = ((COLOUR_PALETTE[palete_index] >> 16) & 0xFF) as u8;
            OUTPUT_RGBA[l+2] = ((COLOUR_PALETTE[palete_index] >> 8) & 0xFF) as u8;
            OUTPUT_RGBA[l+3] = (COLOUR_PALETTE[palete_index] & 0xFF) as u8;
          }
          else {
            let palete_index = (((src_buffer[src_attr_offset] >> 3) & 0x07) |
                                       ((src_buffer[src_attr_offset] & 0x40) >> 3)) as usize;
            OUTPUT_RGBA[l] = ((COLOUR_PALETTE[palete_index] >> 24) & 0xFF) as u8;
            OUTPUT_RGBA[l+1] = ((COLOUR_PALETTE[palete_index] >> 16) & 0xFF) as u8;
            OUTPUT_RGBA[l+2] = ((COLOUR_PALETTE[palete_index] >> 8) & 0xFF) as u8;
            OUTPUT_RGBA[l+3] = (COLOUR_PALETTE[palete_index] & 0xFF) as u8;
          }
          l = l + 4;
        }
      }
    }
  }

  pub fn get_rgba_buffer() -> Box<[u8;SCREEN_SIZE_BYTES]> {
    unsafe {
      let output_rgba = Box::new(OUTPUT_RGBA.clone());
      return output_rgba;
    }
  }

  pub fn get_border_colour() -> Box<[f32;4]> {
    unsafe {
      let mut border_colour_floats = [0.0f32;4];
      border_colour_floats[0] = (((BORDER_COLOR >> 24) & 0xFF) as f32) / 255.0f32;
      border_colour_floats[1] = (((BORDER_COLOR >> 16) & 0xFF) as f32) / 255.0f32;
      border_colour_floats[2] = (((BORDER_COLOR >> 8) & 0xFF) as f32) / 255.0f32;
      border_colour_floats[3] = ((BORDER_COLOR & 0xFF) as f32) / 255.0f32;
      let border_colour = Box::new(border_colour_floats.clone());
      return border_colour;
    }
  }

  pub fn key_down_event(key: &str, shift : bool, sym : bool) {
    //println!("Key down {}", key);
    //println!("Shift {}, Sym {}", shift, sym);
    Keyboard::set_keyboard_key_state(key.to_ascii_uppercase().as_str());
    if shift {
      Keyboard::set_keyboard_key_state("Shift");
    }
    if sym {
      Keyboard::set_keyboard_key_state("Sym");
    }
    //println!("Keyboard state:\n{}", Keyboard);
  }

  pub fn key_up_event(key: &str, shift : bool, sym : bool) {
    //println!("Key up {}", key);
    //println!("Shift {}, Sym {}", shift, sym);
    Keyboard::clear_keyboard_key_state(key.to_ascii_uppercase().as_str());
    if !shift {
      Keyboard::clear_keyboard_key_state("Shift");
    }
    if !sym {
      Keyboard::clear_keyboard_key_state("Sym");
    }
    //println!("Keyboard state:\n{}", Keyboard);
  }


  pub fn create_ula_out_port(request: Receiver<u16>, reply: Sender<u8>) {
    thread::spawn(move || {
      loop {
        let result = request.try_recv();
        match result {
          Ok(addr) => {
            let data = Keyboard::read_port(addr);
            let _ = reply.send(data);
          },
          Err(_) => {}
        }
      }
    });
  }

}