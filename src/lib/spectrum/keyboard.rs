
use std::fmt;
pub struct Keyboard;

pub struct KeyboardRow {
  pub port: u16,
  pub bits: u8
}

pub static mut KEYBOARD_STATE : [KeyboardRow;8] = [
  KeyboardRow { port: 0xFEFE, bits: 0xFF },
  KeyboardRow { port: 0xFDFE, bits: 0xFF },
  KeyboardRow { port: 0xFBFE, bits: 0xFF },
  KeyboardRow { port: 0xF7FE, bits: 0xFF },
  KeyboardRow { port: 0xEFFE, bits: 0xFF },
  KeyboardRow { port: 0xDFFE, bits: 0xFF },
  KeyboardRow { port: 0xBFFE, bits: 0xFF },
  KeyboardRow { port: 0x7FFE, bits: 0xFF }
];

impl Keyboard {

  fn map_symbol_to_number_key(key: &str) -> &str {
    // Note: the spectrum expects numbers and shift keys to be sent seperately
    // but here we get the symbol above the number key.
    // This is for Mac Uk keyboard, it may be different for other keyboards
    match key {
      "!" => "1",
      "@" => "2",
      "£" => "3",
      "$" => "4",
      "%" => "5",
      "^" => "6",
      "&" => "7",
      "*" => "8",
      "(" => "9",
      ")" => "0",
      _ => key
    }
  }

  fn find_key_row_col(key: &str) -> Option<(usize, usize)> {
    let key = Self::map_symbol_to_number_key(key);
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
        KEYBOARD_STATE[row].bits = KEYBOARD_STATE[row].bits & !(0x01 << col);
      }
    }
    else if key.as_bytes()[0] == 0x08 {
      // Delete key
      unsafe {
        KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits & 0xFE;
        KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits & 0xFE;
      }
    }
    else if key.as_bytes().len() == 3 &&
        key.as_bytes()[0] == 0xEF &&
        key.as_bytes()[1] == 0x9C {
      // Arrow keys
      if key.as_bytes()[2] == 0x80 {
        // Up key
        unsafe {
          KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits & 0xF7;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits & 0xFE;
        }
      }
      else if key.as_bytes()[2] == 0x81 {
        // Down key
        unsafe {
          KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits & 0xEF;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits & 0xFE;
        }
      }
      else if key.as_bytes()[2] == 0x82 {
        // Left key
        unsafe {
          KEYBOARD_STATE[3].bits = KEYBOARD_STATE[3].bits & 0xEF;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits & 0xFE;
        }
      }
      else if key.as_bytes()[2] == 0x83 {
        // Right key
        unsafe {
          KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits & 0xFB;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits & 0xFE;
        }
      }
    }
  }

  pub fn clear_keyboard_key_state(key : &str) {
    let location = Self::find_key_row_col(key);
    if location != None {
      let row = location.unwrap().0;
      let col = location.unwrap().1;
      unsafe {
        KEYBOARD_STATE[row].bits = KEYBOARD_STATE[row].bits | (0x01 << col);
      }
    }
    else if key.as_bytes()[0] == 0x08 {
      // Delete key
      unsafe {
        KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits | 0x01;
        KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits | 0x01;
      }
    }
    else if key.as_bytes().len() == 3 &&
        key.as_bytes()[0] == 0xEF &&
        key.as_bytes()[1] == 0x9C {
      // Arrow keys
      if key.as_bytes()[2] == 0x80 {
        // Up key
        unsafe {
          KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits | 0x08;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits | 0x01;
        }
      }
      else if key.as_bytes()[2] == 0x81 {
        // Down key
        unsafe {
          KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits | 0x10;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits | 0x01;
        }
      }
      else if key.as_bytes()[2] == 0x82 {
        // Left key
        unsafe {
          KEYBOARD_STATE[3].bits = KEYBOARD_STATE[3].bits | 0x10;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits | 0x01;
        }
      }
      else if key.as_bytes()[2] == 0x83 {
        // Right key
        unsafe {
          KEYBOARD_STATE[4].bits = KEYBOARD_STATE[4].bits | 0x04;
          KEYBOARD_STATE[0].bits = KEYBOARD_STATE[0].bits | 0x01;
        }
      }
    }
  }

  pub fn read_port(addr : u16) -> u8 {
    unsafe {
      for i in 0..KEYBOARD_STATE.len() {
        if KEYBOARD_STATE[i].port == addr {
          return KEYBOARD_STATE[i].bits;
        }
      }
      return 0xFF;
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
         
