use std::fmt;

pub enum Flag {
  C,    // Carry Flag
  N,    // Add/Subtract Flag
  PV,   // Parity/Overflow Flag
  H,    // Half Carry Flag
  Z,    // Zero Flag
  S     // Sign Flag
}

fn bit_pos_for_flag(flag: Flag) -> u8 {
  match flag {
    Flag::C => 0,
    Flag::N => 1,
    Flag::PV => 2,
    Flag::H => 4,
    Flag::Z => 6,
    Flag::S => 7
  }
}

fn bit_mask_for_flag(flag: Flag) -> u8 {
  match flag {
    Flag::C => 0x01,
    Flag::N => 0x02,
    Flag::PV => 0x04,
    Flag::H => 0x10,
    Flag::Z => 0x40,
    Flag::S => 0x80
  }
}

pub struct Register {
  pub af: u16,
  pub bc: u16,
  pub de: u16,
  pub hl: u16,
  pub af_p: u16,
  pub bc_p: u16,
  pub de_p: u16,
  pub hl_p: u16,
  pub ix: u16,
  pub iy: u16,
  pub sp: u16,
  pub pc: u16,
  pub ir: u16,
  pub wz: u16
}

impl Default for Register {
  fn default() -> Register {
    Register {
      af: 0x0000,
      bc: 0x0000,
      de: 0x0000,
      hl: 0x0000,
      af_p: 0x0000,
      bc_p: 0x0000,
      de_p: 0x0000,
      hl_p: 0x0000,
      ix: 0x0000,
      iy: 0x0000,
      sp: 0x0000,
      pc: 0x0000,
      ir: 0x0000,
      wz: 0x0000
    }
  }
}

impl fmt::Display for Register {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      // Write strictly the first element into the supplied output
      // stream: `f`. Returns `fmt::Result` which indicates whether the
      // operation succeeded or failed. Note that `write!` uses syntax which
      // is very similar to `println!`.
      write!(f, 
        "AF:\t{:04X?}\n\
        BC:\t{:04X?}\n\
        DE:\t{:04X?}\n\
        HL:\t{:04X?}\n\
        AF':\t{:04X?}\n\
        BC':\t{:04X?}\n\
        DE':\t{:04X?}\n\
        HL':\t{:04X?}\n\
        IX:\t{:04X?}\n\
        IY:\t{:04X?}\n\
        SP:\t{:04X?}\n\
        PC:\t{:04X?}\n\
        IR:\t{:04X?}\n\
        WZ:\t{:04X?}\n",
        self.af, self.bc, 
        self.de, self.hl,
        self.af_p, self.bc_p,
        self.de_p, self.hl_p,
        self.ix, self.iy,
        self.sp, self.pc,
        self.ir, self.wz)
  }
}

impl Register {

  pub fn read8(&self, reg_str: &str) -> u8 {
    match reg_str {
      "A" => ((self.af & 0xFF00) >> 8) as u8,
      "F" => ((self.af & 0xFF)) as u8,
      "B" => ((self.bc & 0xFF00) >> 8) as u8,
      "C" => ((self.bc & 0xFF)) as u8,
      "D" => ((self.de & 0xFF00) >> 8) as u8,
      "E" => ((self.de & 0xFF)) as u8,
      "H" => ((self.hl & 0xFF00) >> 8) as u8,
      "L" => ((self.hl & 0xFF)) as u8,
      "A'" => ((self.af_p & 0xFF00) >> 8) as u8,
      "F'" => ((self.af_p & 0xFF)) as u8,
      "B'" => ((self.bc_p & 0xFF00) >> 8) as u8,
      "C'" => ((self.bc_p & 0xFF)) as u8,
      "D'" => ((self.de_p & 0xFF00) >> 8) as u8,
      "E'" => ((self.de_p & 0xFF)) as u8,
      "H'" => ((self.hl_p & 0xFF00) >> 8) as u8,
      "L'" => ((self.hl_p & 0xFF)) as u8,
      "I" => ((self.ir & 0xFF00) >> 8) as u8,
      "R'" => ((self.ir & 0xFF)) as u8,
      _ => {
        println!("Can't read to register {}", reg_str);
        0x00
      }
    }
  }

  pub fn read16(&self, reg_str: &str) -> u16 {
    match reg_str {
      "AF" => self.af,
      "BC" => self.bc,
      "DE" => self.de,
      "HL" => self.hl,
      "AF'" => self.af_p,
      "BC'" => self.bc_p,
      "DE'" => self.de_p,
      "HL'" => self.hl_p,
      "IX" => self.ix,
      "IY" => self.iy,
      "SP" => self.sp,
      "PC" => self.pc,
      "IR" => self.ir,
      "WZ" => self.wz,
      _ => { 
        println!("Can't read to register {}", reg_str);
        0x0000
      }
    }
  }

  pub fn write8(&mut self, reg_str: &str, val: u8) {
    match reg_str {
      "A" => { self.af = (self.af & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "F" => { self.af = (self.af & 0xFF00) | ((val as u16) & 0x00FF); },
      "B" => { self.bc = (self.bc & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "C" => { self.bc = (self.bc & 0xFF00) | ((val as u16) & 0x00FF); },
      "D" => { self.de = (self.de & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "E" => { self.de = (self.de & 0xFF00) | ((val as u16) & 0x00FF); },
      "H" => { self.hl = (self.hl & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "L" => { self.hl = (self.hl & 0xFF00) | ((val as u16) & 0x00FF); },
      "A'" => { self.af_p = (self.af_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "F'" => { self.af_p = (self.af_p & 0xFF00) | ((val as u16) & 0x00FF); },
      "B'" => { self.bc_p = (self.bc_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "C'" => { self.bc_p = (self.bc_p & 0xFF00) | ((val as u16) & 0x00FF); },
      "D'" => { self.de_p = (self.de_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "E'" => { self.de_p = (self.de_p & 0xFF00) | ((val as u16) & 0x00FF); },
      "H'" => { self.hl_p = (self.hl_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "L'" => { self.hl_p = (self.hl_p & 0xFF00) | ((val as u16) & 0x00FF); },
      "I" => { self.ir = (self.ir & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "R" => { self.ir = (self.ir & 0xFF00) | ((val as u16) & 0x00FF); },
      _ => println!("Can't write to register {}", reg_str)
    }
  }

  pub fn write16(&mut self, reg_str: &str, val: u16) {
    match reg_str {
      "AF" => { self.af = val; },
      "BC" => { self.bc = val; },
      "DE" => { self.de = val; },
      "HL" => { self.hl = val; },
      "AF'" => { self.af_p = val; },
      "BC'" => { self.bc_p = val; },
      "DE'" => { self.de_p = val; },
      "HL'" => { self.hl_p = val; },
      "IX" => { self.ix = val; },
      "IY" => { self.iy = val; },
      "SP" => { self.sp = val; },
      "PC" => { self.pc = val; },
      "IR" => { self.ir = val; },
      "WZ" => { self.wz = val; },
      _ => println!("Can't write to register {}", reg_str)
    }
  }

  pub fn set_flag(&mut self, flag: Flag) {
    let mut f_reg = (self.af & 0xFF) as u8;
    f_reg = f_reg | bit_mask_for_flag(flag);
    self.af = self.af | (f_reg as u16);
  }

  pub fn clear_flag(&mut self, flag: Flag) {
    let mut f_reg = (self.af & 0xFF) as u8;
    f_reg = f_reg & !bit_mask_for_flag(flag);
    self.af = (self.af & 0xFF00) | (f_reg as u16);
  }
}

