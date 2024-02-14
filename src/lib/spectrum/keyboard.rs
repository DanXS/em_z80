
use std::fmt;
pub struct Keyboard;

pub struct KeyboardRow {
  pub port: u16,
  pub bits: u8
}

pub static mut KEYBOARD_STATE : [KeyboardRow;8] = [
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 },
  KeyboardRow { port: 0xFEFE, bits: 0x00 }
];

impl Keyboard {

  fn find_key_row_col(key: &str) -> Option<(usize, usize)> {
    let keyboard_map : [[&'static str;5];8] = [
      ["Shift", "Z", "X", "C", "V"],
      ["A", "S", "D", "F", "G"],
      ["Q", "W", "E", "R", "T"],
      ["1", "2", "3", "4", "5"],
      ["0", "9", "8", "7", "6"],
      ["P", "O", "I", "U", "Y"],
      ["\n", "L", "K", "J", "H"],
      [" ", "Sym", "M", "N", "B"],
    ];
    for row in 0..8 {
      for col in 0..5 {
        if keyboard_map[row][col] == key {
          return Some((row, col));
        }
      }
    }
    None
  }

  pub fn set_keyboard_key_state(key : &str) {
    let location = Self::find_key_row_col(key);
    if location != None {
      let row = location.unwrap().0;
      let col = location.unwrap().1;
      unsafe {
        KEYBOARD_STATE[row].bits = KEYBOARD_STATE[row].bits | (0x01 << col);
      }
    }
  }

  pub fn clear_keyboard_key_state(key : &str) {
    let location = Self::find_key_row_col(key);
    if location != None {
      let row = location.unwrap().0;
      let col = location.unwrap().1;
      unsafe {
        KEYBOARD_STATE[row].bits = KEYBOARD_STATE[row].bits & !(0x01 << col);
      }
    }
  }
}

impl fmt::Display for Keyboard {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    unsafe {
      write!(f, "{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n{:08b}\n", 
        KEYBOARD_STATE[0].bits,
        KEYBOARD_STATE[1].bits,
        KEYBOARD_STATE[2].bits,
        KEYBOARD_STATE[3].bits,
        KEYBOARD_STATE[4].bits,
        KEYBOARD_STATE[5].bits,
        KEYBOARD_STATE[6].bits,
        KEYBOARD_STATE[7].bits)
    }
  }
}
         

                
                
