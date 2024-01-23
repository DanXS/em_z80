
use std::io;
use std::fmt;
use std::io::prelude::*;
use std::fs::File;
use std::sync::Mutex;

pub const MEM_SIZE : u32 = 0x10000;
pub const ROM_SIZE : u32 = 0x4000;

static mut BYTES : Mutex<[u8; MEM_SIZE as usize]> = Mutex::new([0; MEM_SIZE as usize]);

pub struct Memory;

impl fmt::Display for Memory {

  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    for i in (0..MEM_SIZE-1).step_by(16) {
      let res = match write!(f, "{:04X?}", i) {
        Ok(res) => res,
        Err(e) => return Err(e),
      };
      for j in 0..15 {
        unsafe {
          let byte =  BYTES.lock().unwrap()[(i+j) as usize];
          let res = match write!(f, " {:02X?}", byte) {
            Ok(res) => res,
            Err(e) => return Err(e),
          };
        }
      }
      let res = match write!(f, "\n") {
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

  pub fn load_rom(filename : &str) -> io::Result<()> {
    println!("Loading ROM: {}", filename);
    let mut f = File::open(filename)?;
    let mut buffer = [0; ROM_SIZE as usize];
    let n = f.read(&mut buffer[..])?;
    unsafe {
      BYTES.lock().unwrap()[..n].clone_from_slice(&buffer);
    }
    Ok(())
  }
}
