mod keyboard;
mod ula;

use ula::DataBus;
use ula::Ula;
use ula::SCREEN_SIZE_BYTES;
use std::sync::mpsc::{Sender, Receiver};

pub fn next_vblank() -> u8 {
    let fut = Ula::next_vblank(&DataBus{value: 0x38});
    fut().unwrap()
}

pub fn convert_screen_buffer_rgba(src_buffer : &[u8]) {
  unsafe {
    let restore_state = critical_section::acquire();
    Ula::convert_screen_buffer_rgba(src_buffer);
    critical_section::release(restore_state);
  }
}

pub fn get_rgba_buffer() -> Box<[u8; SCREEN_SIZE_BYTES]> {
  unsafe {
    let restore_state = critical_section::acquire();
    let res = Ula::get_rgba_buffer();
    critical_section::release(restore_state);
    return res;
  }
}

pub fn get_border_colour() -> Box<[f32;4]> {
  unsafe {
    let restore_state = critical_section::acquire();
    let res = Ula::get_border_colour();
    critical_section::release(restore_state);
    return res;
  }
}

pub fn key_down_event(key: &str, shift : bool, sym : bool) {
  Ula::key_down_event(key, shift, sym);
}

pub fn key_up_event(key: &str, shift : bool, sym : bool) {
  Ula::key_up_event(key, shift, sym);
}

pub fn create_ula_out_port(request: Receiver<u16>, reply: Sender<u8>) {
  Ula::create_ula_out_port(request, reply);
}