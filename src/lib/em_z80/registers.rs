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

#[derive(Clone, Copy)]
pub enum RegID {
  A, F, B, C, D, E, H, L,
  AP, FP, BP, CP, DP, EP, HP, LP, 
  I, R, IXH, IXL, IYH, IYL,
  AF, BC, DE, HL,
  AFP, BCP, DEP, HLP,
  IX, IY, SP, PC, IR, WZ
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

  pub fn read8(&self, reg: RegID) -> u8 {
    match reg {
      RegID::A => ((self.af & 0xFF00) >> 8) as u8,
      RegID::F => ((self.af & 0xFF)) as u8,
      RegID::B => ((self.bc & 0xFF00) >> 8) as u8,
      RegID::C => ((self.bc & 0xFF)) as u8,
      RegID::D => ((self.de & 0xFF00) >> 8) as u8,
      RegID::E => ((self.de & 0xFF)) as u8,
      RegID::H => ((self.hl & 0xFF00) >> 8) as u8,
      RegID::L => ((self.hl & 0xFF)) as u8,
      RegID::AP => ((self.af_p & 0xFF00) >> 8) as u8,
      RegID::FP=> ((self.af_p & 0xFF)) as u8,
      RegID::BP=> ((self.bc_p & 0xFF00) >> 8) as u8,
      RegID::CP => ((self.bc_p & 0xFF)) as u8,
      RegID::DP => ((self.de_p & 0xFF00) >> 8) as u8,
      RegID::EP => ((self.de_p & 0xFF)) as u8,
      RegID::HP => ((self.hl_p & 0xFF00) >> 8) as u8,
      RegID::LP => ((self.hl_p & 0xFF)) as u8,
      RegID::I  => ((self.ir & 0xFF00) >> 8) as u8,
      RegID::R  => ((self.ir & 0xFF)) as u8,
      RegID::IXH => ((self.ix & 0xFF00) >> 8) as u8,
      RegID::IXL => ((self.ix & 0xFF)) as u8,
      RegID::IYH => ((self.iy & 0xFF00) >> 8) as u8,
      RegID::IYL => ((self.iy & 0xFF)) as u8,
      _ => panic!("Invalid 8 bit register calling read8!")
    }
  }

  pub fn read16(&self, reg: RegID) -> u16 {
    match reg {
      RegID::AF => self.af,
      RegID::BC => self.bc,
      RegID::DE => self.de,
      RegID::HL => self.hl,
      RegID::AFP => self.af_p,
      RegID::BCP => self.bc_p,
      RegID::DEP => self.de_p,
      RegID::HLP => self.hl_p,
      RegID::IX => self.ix,
      RegID::IY => self.iy,
      RegID::SP => self.sp,
      RegID::PC => self.pc,
      RegID::IR => self.ir,
      RegID::WZ => self.wz,
      _ => panic!("Invalid 16 bit register calling read16!")
    }
  }

  pub fn write8(&mut self, reg: RegID, val: u8) {
    match reg {
      RegID::A => { self.af = (self.af & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::F => { self.af = (self.af & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::B => { self.bc = (self.bc & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::C => { self.bc = (self.bc & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::D => { self.de = (self.de & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::E => { self.de = (self.de & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::H => { self.hl = (self.hl & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::L => { self.hl = (self.hl & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::AP => { self.af_p = (self.af_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::FP => { self.af_p = (self.af_p & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::BP => { self.bc_p = (self.bc_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::CP => { self.bc_p = (self.bc_p & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::DP => { self.de_p = (self.de_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::EP => { self.de_p = (self.de_p & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::HP => { self.hl_p = (self.hl_p & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::LP => { self.hl_p = (self.hl_p & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::I => { self.ir = (self.ir & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::R => { self.ir = (self.ir & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::IXH => { self.ix = (self.ix & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::IXL => { self.ix = (self.ix & 0xFF00) | ((val as u16) & 0x00FF); },
      RegID::IYH => { self.iy = (self.iy & 0x00FF) | (((val as u16) << 8) & 0xFF00); },
      RegID::IYL => { self.iy = (self.iy & 0xFF00) | ((val as u16) & 0x00FF); },
      _ => panic!("Invalid 8 bit register calling write8!")
    }
  }

  pub fn write16(&mut self, reg: RegID, val: u16) {
    match reg {
      RegID::AF => { self.af = val; },
      RegID::BC => { self.bc = val; },
      RegID::DE => { self.de = val; },
      RegID::HL => { self.hl = val; },
      RegID::AFP => { self.af_p = val; },
      RegID::BCP => { self.bc_p = val; },
      RegID::DEP => { self.de_p = val; },
      RegID::HLP => { self.hl_p = val; },
      RegID::IX => { self.ix = val; },
      RegID::IY => { self.iy = val; },
      RegID::SP => { self.sp = val; },
      RegID::PC => { self.pc = val; },
      RegID::IR => { self.ir = val; },
      RegID::WZ => { self.wz = val; },
      _ => panic!("Invalid 16 bit register calling write16!")
    }
  }

  pub fn from_str(reg_str: &str) -> RegID {
    match reg_str {
      "A" => RegID::A,
      "F" => RegID::F,
      "B" => RegID::B,
      "C" => RegID::C,
      "D" => RegID::D,
      "E" => RegID::E,
      "H" => RegID::H,
      "L" => RegID::L,
      "A'" => RegID::AP,
      "F'" => RegID::FP,
      "B'" => RegID::BP,
      "C'" => RegID::CP,
      "D'" => RegID::DP,
      "E'" => RegID::EP,
      "H'" => RegID::HP,
      "L'" => RegID::LP,
      "I" => RegID::I,
      "R" => RegID::R,
      "IXH" => RegID::IXH,
      "IXL" => RegID::IXL,
      "IYH" => RegID::IYH,
      "IYL" => RegID::IYL,
      "AF" => RegID::AF,
      "BC" => RegID::BC,
      "DE" => RegID::DE,
      "HL" => RegID::HL,
      "AF'" => RegID::AFP,
      "BC'" => RegID::BCP,
      "DE'" => RegID::DEP,
      "HL'" => RegID::HLP,
      "IX" => RegID::IX,
      "IY" => RegID::IY,
      "SP" => RegID::SP,
      "PC" => RegID::PC,
      "IR" => RegID::IR,
      "WZ" => RegID::WZ,
      _ => panic!("Invalid Register")
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

