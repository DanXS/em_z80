use std::fmt;
use std::io::prelude::*;
use std::fs::File;
use std::sync::Mutex;
use std::io::ErrorKind;

pub const MEM_SIZE : u32 = 0x10000;
pub const ROM_SIZE : u32 = 0x4000;

static mut BYTES : Mutex<[u8; MEM_SIZE as usize]> = Mutex::new([0; MEM_SIZE as usize]);

pub struct Memory;

impl fmt::Display for Memory {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    for i in (0..MEM_SIZE-1).step_by(16) {
      match write!(f, "{:04X?}: ", i) {
        Ok(res) => res,
        Err(e) => return Err(e),
      };
      for j in 0..16 {
        unsafe {
          let byte =  BYTES.lock().unwrap()[(i+j) as usize];
          match write!(f, "{:02X?} ", byte) {
            Ok(res) => res,
            Err(e) => return Err(e),
          };
        }
      }
      for j in 0..16 {
        unsafe {
          let byte =  BYTES.lock().unwrap()[(i+j) as usize];
          let mut chr = '.';
          if byte.is_ascii() && !byte.is_ascii_control() {
            chr = byte as char;
          };
          match write!(f, "{}", chr.to_string()) {
            Ok(res) => res,
            Err(e) => return Err(e),
          };
        }
      }
      match write!(f, "\n") {
        Ok(res) => res,
        Err(e) => return Err(e),
      };
    }
    Ok(())
  }
}

impl Memory {

  pub fn read8(address : u16) -> u8 {
    unsafe {
      BYTES.lock().unwrap()[address as usize]
    }
  }

  pub fn read16(address: u16) -> u16 {
    let val1 = Self::read8(address);
    if (address as u32) == MEM_SIZE-1 {
      let val2 = Self::read8(0);
      ((val1 as u16) & 0xFF) | (((val2 as u16) << 8) & 0xFF00)
    }
    else {
      let val2 = Self::read8(address+1);
      ((val1 as u16) & 0xFF) | (((val2 as u16) << 8) & 0xFF00)
    }
  }

  pub fn write8(address : u16, val: u8) {
    unsafe {
      // Only write to RAM
      if (address as u32) >= ROM_SIZE {
        BYTES.lock().unwrap()[address as usize] = val;
      }
    }
  }

  pub fn write16(address: u16, val: u16) {
    Self::write8(address, (val & 0xFF) as u8);
    if (address as u32) == MEM_SIZE-1 {
      Self::write8(0, ((val >> 8) & 0xFF) as u8);
    }
    else {
      Self::write8(address+1, ((val >> 8) & 0xFF) as u8);
    }
  }

  pub fn load_bin(filename : &str, start_addr: usize) -> std::io::Result<()> {
    println!("Loading ROM: {}", filename);
    let mut f = File::open(filename)?;
    let metadata = f.metadata()?;
    let len = metadata.len() ;
    if start_addr as u32 + len as u32 > MEM_SIZE {
      let error =  std::io::Error::new(ErrorKind::OutOfMemory, "The file will extend behond the top of memory if it loaded at that location!");
      return Err(error);
    }
    else {
      unsafe {
        f.read(&mut (BYTES.lock().unwrap()[start_addr..]))?;
      }
    }
    Ok(())
  }

  pub fn read_slice(start_addr: usize, end_addr: usize, buffer: &mut Vec<u8>) {
    assert!(start_addr < end_addr);
    assert!(end_addr < MEM_SIZE as usize);
    unsafe {
      (BYTES.lock().unwrap()[start_addr..end_addr]).clone_into(buffer);
    }
  }
}
