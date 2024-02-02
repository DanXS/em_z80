use core::panic;
use std::fmt;

pub enum Flag {
  C,    // Carry Flag
  N,    // Add/Subtract Flag
  PV,   // Parity/Overflow Flag
  H,    // Half Carry Flag
  Z,    // Zero Flag
  S     // Sign Flag
}

pub fn bit_mask_for_flag(flag: Flag) -> u8 {
  match flag {
    Flag::C => 0x01,
    Flag::N => 0x02,
    Flag::PV => 0x04,
    Flag::H => 0x10,
    Flag::Z => 0x40,
    Flag::S => 0x80
  }
}

pub enum ConditionCode {
  NZ = 0x00,  // 000 Non-Zero (NZ) Z
  Z  = 0x01,  // 001 Zero (Z) Z
  NC = 0x02,  // 010 Non Carry (NC) C
  C  = 0x03,  // 011 Carry (C) Z
  PO = 0x04,  // 100 Parity Odd (PO) P/V
  PE = 0x05,  // 101 Parity Even (PE) P/V
  P  = 0x06,  // 110 Sign Positive (P) S
  M  = 0x07   // 111 Sign Negative (M) S
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
      write!(f, 
        "AF: {:04X?}  \
        AF': {:04X?}\n\
        BC: {:04X?}  \
        BC': {:04X?}\n\
        DE: {:04X?}  \
        DE': {:04X?}\n\
        HL: {:04X?}  \
        HL': {:04X?}\n\
        IX: {:04X?}   \
        IY: {:04X?}\n\
        SP: {:04X?}   \
        PC: {:04X?}\n\
        IR: {:04X?}   \
        WZ: {:04X?}\n",
        self.af, self.af_p, 
        self.bc, self.bc_p,
        self.de, self.de_p,
        self.hl, self.hl_p,
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
      "IXH" => ((self.ix & 0xFF00) >> 8) as u8,
      "IXL" => ((self.ix & 0xFF)) as u8,
      "IYH" => ((self.iy & 0xFF00) >> 8) as u8,
      "IYL" => ((self.iy & 0xFF)) as u8,
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
      "IXH" => { self.ix = (self.ix & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "IXL" => { self.ix = (self.ix & 0xFF00) | ((val as u16) & 0x00FF); },
      "IYH" => { self.iy = (self.iy & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      "IYL" => { self.iy = (self.iy & 0xFF00) | ((val as u16) & 0x00FF); },
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

  pub fn is_flag_set(&mut self, flag: Flag) -> bool {
    let f_reg = (self.af & 0xFF) as u8;
    f_reg & bit_mask_for_flag(flag) != 0x00
  }

  pub fn condition_code_for(val : u8) -> ConditionCode {
    match val {
      0 => ConditionCode::NZ,
      1 => ConditionCode::Z,
      2 => ConditionCode::NC,
      3 => ConditionCode::C,
      4 => ConditionCode::PO,
      5 => ConditionCode::PE,
      6 => ConditionCode::P,
      7 => ConditionCode::M,
      _ => panic!("Condition code not found, should be a value between 0 and 7")
    }
  }

  pub fn check_condition(cc : ConditionCode, flags: u8) -> bool {
    match cc {
      ConditionCode::NZ => bit_mask_for_flag(Flag::Z) & flags == 0,
      ConditionCode::Z  => bit_mask_for_flag(Flag::Z) & flags != 0,
      ConditionCode::NC => bit_mask_for_flag(Flag::C) & flags == 0,
      ConditionCode::C  => bit_mask_for_flag(Flag::C) & flags != 0,
      ConditionCode::PO => bit_mask_for_flag(Flag::PV) & flags == 0,
      ConditionCode::PE => bit_mask_for_flag(Flag::PV) & flags != 0,
      ConditionCode::P  => bit_mask_for_flag(Flag::S) & flags == 0,
      ConditionCode::M  => bit_mask_for_flag(Flag::S) & flags != 0
    }
  }
}

