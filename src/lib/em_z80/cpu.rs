
use std::cmp::max;
use std::thread;
use core::time::Duration;
use std::time::Instant;
use crate::util::*;
use crate::instructions::Instruction;
use crate::opcodes;
use crate::opcodes::Opcode;
use crate::memory::Memory;
use crate::registers::Register;
use crate::registers::Flag;
use crate::registers::RegID;

pub static mut REG : Register = Register {
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
};


#[allow(dead_code)]
pub enum InterruptMode {
  Zero,
  One,
  Two
}

struct CpuState {
  pub cycle_time: u64,
  pub active_cycles: u8,
  pub breakpoints: Vec<u16>,
  pub breakpoints_enabled: bool,
  pub is_running: bool,
  pub databus_val: u8,
  pub interrupt_mode: InterruptMode,
  pub interrupt_iff1: bool,
  pub interrupt_iff2: bool,
  pub is_cpu_halted: bool,
  pub interrupt_triggered: bool
}

static mut CPU_STATE : CpuState = CpuState { 
  cycle_time: 286,
  active_cycles: 0,
  breakpoints: Vec::new(),
  breakpoints_enabled: false,
  is_running: false,
  databus_val: 0x00,
  interrupt_mode: InterruptMode::One,
  interrupt_iff1: false,
  interrupt_iff2: false,
  is_cpu_halted: false,
  interrupt_triggered: false
};

pub struct Cpu;

impl Cpu {

  #[inline]
  pub fn read_reg8(reg: RegID) -> u8 {
    unsafe {
      REG.read8(reg)
    }
  }

  #[inline]
  pub fn read_reg16(reg: RegID) -> u16 {
    unsafe {
      REG.read16(reg)
    }
  }

  #[inline]
  pub fn write_reg8(reg: RegID, val: u8) {
    unsafe {
      REG.write8(reg, val);
    }
  }

  #[inline]
  pub fn write_reg16(reg: RegID, val: u16) {
    unsafe {
      REG.write16(reg, val);
    }
  }

  #[inline]
  pub fn set_flag(flag: Flag) {
    unsafe {
      REG.set_flag(flag);
    }
  }

  #[inline]
  pub fn clear_flag(flag: Flag) {
    unsafe {
      REG.clear_flag(flag);
    }
  }

  #[inline]
  pub fn is_flag_set(flag: Flag) -> bool {
    unsafe {
      REG.is_flag_set(flag)
    }
  }

  #[inline]
  pub fn update_flags_for_inc(val : u8, inc_val : u8) {
    unsafe {
      REG.update_flags_for_inc(val, inc_val);
    }
  }

  #[inline]
  pub fn update_flags_for_dec(val : u8, dec_val : u8) {
    unsafe {
      REG.update_flags_for_dec(val, dec_val);
    }
  }

  #[inline]
  pub fn update_flags_for_addition8(val1 : u8, val2: u8, res: u16) {
    unsafe {
      REG.update_flags_for_addition8(val1, val2, res);
    }
  }

  #[inline]
  pub fn update_flags_for_subtraction8(val1 : u8, val2: u8, res: u16) {
    unsafe {
      REG.update_flags_for_subtraction8(val1, val2, res);
    }
  }

  #[inline]
  pub fn update_flags_for_addition16(val1: u16, res: u32) {
    unsafe {
      REG.update_flags_for_addition16(val1, res);
    }
  }

  #[inline]
  pub fn update_flags_for_addition_with_carry16(val1: u16, val2: u16, res: u32) {
    unsafe {
      REG.update_flags_for_addition_with_carry16(val1, val2, res);
    }
  }

  #[inline]
  pub fn update_flags_for_subtraction_with_carry16(val1 : u16, val2: u16, res : u32) {
    unsafe {
      REG.update_flags_for_subtraction_with_carry16(val1, val2, res);
    }
  }

  #[inline]
  pub fn update_flags_for_negation8(val: u8, res: u16) {
    unsafe {
      REG.update_flags_for_negation8(val, res);
    }
  }

  #[inline]
  pub fn update_flags_for_logical_op(res: u8, set_h: bool) {
    unsafe {
      REG.update_flags_for_logical_op(res, set_h);
    }
  }

  #[inline]
  pub fn update_flags_for_compare8(val1: u8, val2: u8) {
    unsafe {
      REG.update_flags_for_compare8(val1, val2);
    }
  }

  #[inline]
  pub fn update_flags_shift_left(val: u8, res: u8) {
    unsafe {
      REG.update_flags_shift_left(val, res);
    }
  }

  #[inline]
  pub fn update_flags_shift_right(val: u8, res: u8) {
    unsafe {
      REG.update_flags_shift_right(val, res);
    }
  }

  #[inline]
  pub fn get_register_view_string() -> String {
    unsafe {
      REG.to_string()
    }
  }

  pub fn get_pc() -> u16 {
    Self::read_reg16(RegID::PC)
  }

  pub fn set_pc(addr: u16) {
    Self::write_reg16(RegID::PC, addr);
  }

  pub fn push_pc() {
    let pc = Self::get_pc();
    let mut addr = Self::read_reg16(RegID::SP);
    addr = ((addr as i32) - 2) as u16;
    Self::write_reg16(RegID::SP, addr);
    Memory::write16(addr, pc);
  }

  pub fn pop_pc() {
    let mut addr = Self::read_reg16(RegID::SP);
    let pc = Memory::read16(addr);
    addr = ((addr as i32) + 2) as u16;
    Self::write_reg16(RegID::SP, addr);
    Self::set_pc(pc);
  }

  pub fn get_register8(reg: RegID) -> u8 {
    Self::read_reg8(reg)
  }

  pub fn set_register8(reg: RegID, val: u8) {
    Self::write_reg8(reg, val);
  }

  pub fn get_register16(reg: RegID) -> u16 {
    Self::read_reg16(reg)
  }

  pub fn set_register16(reg: RegID, val: u16) {
    Self::write_reg16(reg, val);
  }

  pub fn set_status_flag(flag: Flag) {
    Self::set_flag(flag);
  }
  
  pub fn clear_status_flag(flag: Flag) {
    Self::clear_flag(flag);
  }
  
  pub fn get_status_flag(flag: Flag) -> bool {
    Self::is_flag_set(flag)
  }

  pub fn get_register_from_str(reg_str: &str) -> RegID {
    Register::from_str(reg_str)
  }

  pub fn set_cpu_frequency(mhz: f32) {
    if mhz <= 0.0f32 {
      panic!("Attempt to set cpu frequency to zero or negative value, please pass a positive value only!");
    }
    else {
      let ns = (1.0f32/mhz*1e3) as u64;
      println!("CPU frequency {}mhz, cycle time {}ns", mhz, ns);
      Cpu::set_cycle_time(ns);
    }
  }

  pub fn get_cycle_time() -> u64 {
    unsafe {
      CPU_STATE.cycle_time
    }
  }

  pub fn set_cycle_time(ns: u64) {
    unsafe {
      CPU_STATE.cycle_time = ns;
    }
  }

  pub fn get_acitve_cycles() -> u8 {
    unsafe {
      CPU_STATE.active_cycles
    }
  }

  pub fn set_acitve_cycles(cycles: u8) {
    unsafe {
      CPU_STATE.active_cycles = cycles;
    }
  }

  pub fn enable_interrupts() {
    unsafe {
      CPU_STATE.interrupt_iff1 = true;
      CPU_STATE.interrupt_iff2 = true;
    }
  }

  pub fn disable_interrupts() {
    unsafe {
      CPU_STATE.interrupt_iff1 = false;
      CPU_STATE.interrupt_iff2 = false;
    }
  }

  pub fn set_interrupt_mode(mode: InterruptMode) {
    unsafe {
      CPU_STATE.interrupt_mode = mode;
    }
  }

  pub fn halt_until_interrupt() {
    unsafe {
      CPU_STATE.is_cpu_halted = true;
    }
  }

  pub fn return_from_interrupt() {
    unsafe {
      Self::pop_pc();
      CPU_STATE.interrupt_triggered = false;
    }
  }

  pub fn return_from_non_maskable_interrupt() {
    unsafe {
      Self::pop_pc();
      CPU_STATE.interrupt_iff1 = CPU_STATE.interrupt_iff2;
    }
  }

  pub fn toggle_breakpoint(addr: u16) {
    unsafe {
      let mut bps = CPU_STATE.breakpoints.clone();
      match bps.binary_search(&addr) {
        Ok(pos) => {
            println!("Removing breakpoint {} at position {}", addr, pos);
            bps.remove(pos);
            CPU_STATE.breakpoints = bps;
        },
        Err(pos) => {
            println!("Adding breakpoint {} at position {}", addr, pos);
            bps.insert(pos, addr.clone());
            CPU_STATE.breakpoints = bps;
        }
      }
    }
  }

  pub fn update_breakpoints_enabled(enabled : bool) {
    unsafe {
      CPU_STATE.breakpoints_enabled = enabled;
    }
  }

  pub fn has_breakpoint(addr: u16) -> bool {
    unsafe{
      CPU_STATE.breakpoints.contains(&addr)
    }
  }

  pub fn disassemble(addr: u16) -> (String, u8) {
    // Fetch the opcode from the opcode tables
    let (opcode, data, n) = Self::fetch(addr);
    let inst = opcode.instruction;
    // If the operand is two bytes long replace the 'nn' of the instruction by the
    // two data bytes
    if n == 2 {
      if inst.contains("nn") {
        let s = format!("{:02X?}{:02X?}H", data[1], data[0]);
        (inst.replace("nn", s.as_str()), opcode.byte_count)
      }
      else {
        let s0 = format!("{:02X?}H", data[0]);
        let s1 = format!("{:02X?}H", data[1]);
        (inst.replace("d", s0.as_str()).replace("n", s1.as_str()), opcode.byte_count)
      }
    }
    else if n == 1 {
        // Otherwise if the instruction has one byte of data
        // replace the 'n' or 'd' by the single byte of data
        let s = format!("{:02X?}H", data[0]);
        if inst.contains("n") {
            (inst.replace("n", s.as_str()), opcode.byte_count)
        }
        else if inst.contains("d") {
            (inst.replace("d", s.as_str()), opcode.byte_count)
        }
        else {
            (String::from(inst), opcode.byte_count)
        }
    }
    else {
        (String::from(inst), opcode.byte_count)
    }
  }

  fn fetch(mut address: u16) -> (&'static Opcode<'static>, [u8;2], usize)
  {
      // Read the first opcode byte
      let code = Memory::read8(address);
      let mut opcode = &opcodes::MAIN_OPCODES[code as usize];
      let mut is_index_bit_table = false;
      let mut data: [u8;2] = [0,0];
      // Increment address
      address = inc_u16_wrap(address);
      let mut inst_len : usize = 1;
      // Check for alternate multibyte opcode
      if opcode.instruction == "MISC" {
        // It's a two byte opcode from MISC table
        let code = Memory::read8(address);
        opcode = &opcodes::MISC_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
      }
      else if opcode.instruction == "BIT" {
        // It's a two byte opcode from BIT table
        let code = Memory::read8(address);
        opcode = &opcodes::BIT_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
      }
      else if opcode.instruction == "IX" {
        // It's a two byte opcode from IX table
        let code = Memory::read8(address);
        opcode = &opcodes::IX_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
        if opcode.instruction == "IX_BIT" {
          // It's a three byte opcode from IX_BIT table
          // Here the data comes before the opcode
          is_index_bit_table = true;
          data[0] = Memory::read8(address);
          address = inc_u16_wrap(address);
          let code = Memory::read8(address);
          opcode = &opcodes::IX_BIT_OPCODES[code as usize];
          address = inc_u16_wrap(address);
          inst_len = 3;
        }
      }
      else if opcode.instruction == "IY" {
        // It's a two byte opcode from IY table
        let code = Memory::read8(address);
        opcode = &opcodes::IY_OPCODES[code as usize];
        address = inc_u16_wrap(address);
        inst_len = 2;
        if opcode.instruction == "IY_BIT" {
          // It's a three byte opcode from IY_BIT table
          // Here the data comes before the opcode
          is_index_bit_table = true;
          data[0] = Memory::read8(address);
          address = inc_u16_wrap(address);
          let code = Memory::read8(address);
          opcode = &opcodes::IY_BIT_OPCODES[code as usize];
          address = inc_u16_wrap(address);
          inst_len = 3;
        }
      }
      // Read the remaining bytes of data for the operand's
      let n = (opcode.byte_count as usize) - inst_len;
      if !is_index_bit_table {
        // IX_BIT and IY_BIT tables have data before opcode so ignore those cases
        // as we already have the data, otherwise read the remaining operand data
        for i in 0..n {
          data[i] = Memory::read8(address);
          address = inc_u16_wrap(address);
        }
      }
      (opcode, data, n)
  }

  pub fn step() {
    // The minimum number of cycles unless changed by the instruction itself
    // For example, branching may take more cycles (cycles.0)
    let now = Instant::now();

    // Fetch the instruction
    let (opcode, data, n) = Self::fetch(Self::get_pc());
    // Increment the program counter
    Self::set_pc((((Self::get_pc() as u32) + (opcode.byte_count as u32)) & 0xFFFF) as u16);
    // Run the instruction
    Self::run_inst(opcode.func, opcode, data, n);

    // Get the elapsed time the instruction took to execute
    let elapsed = now.elapsed().as_nanos() as u64;
    println!("Elapsed time without delay {}ns", elapsed);
    // Simulate the time it takes for the instruciton to run
    let delay_ns = Self::get_cycle_time()*(Self::get_acitve_cycles() as u64);
    println!("Cycles to execute: {}, delay {}ns", Self::get_acitve_cycles(), delay_ns);
    let sleep_time_ns = max(0, (delay_ns as i64) - (elapsed as i64));
    thread::sleep(Duration::from_nanos(sleep_time_ns as u64));
  }

  fn run_inst(f: fn(&Instruction), opcode: &Opcode, data: [u8;2], len: usize) {
    Self::set_acitve_cycles(opcode.cycles.1);
    // Construct the instruction
    let inst = Instruction{code: opcode.code, 
      data: ((data[1] as u16) << 8) | (data[0] as u16), 
      len: len, table: opcode.table,
      cycles: opcode.cycles};
    // Run the instruction
    f(&inst);
  }

  pub fn run() {
    unsafe { CPU_STATE.is_running = true };
    while unsafe { CPU_STATE.is_running } {
      unsafe {
        if CPU_STATE.is_cpu_halted {
          continue;
        }
      }
      let (text, _) = Self::disassemble(Self::get_pc());
      println!("Step:\n{:04X?}: {} ", Self::get_pc(), text);
      Self::step();
      if unsafe { CPU_STATE.breakpoints_enabled } {
        if unsafe { CPU_STATE.breakpoints.contains(&Self::get_pc()) } {
          println!("Hit breakpoint at: {:04X?}", &Self::get_pc());
          break;
        }
      }
      unsafe {
        // Handle interrupts
        if  CPU_STATE.interrupt_triggered  {
          if matches!(CPU_STATE.interrupt_mode, InterruptMode::One) {
            let isr_addr = CPU_STATE.databus_val as u16;
            // Save the program counter to the stack
            Self::push_pc();
            Self::set_pc(isr_addr);
            println!("Servicing Interrupt in Mode One");
            CPU_STATE.is_cpu_halted = false;
          }
          else {
            println!("Interrupt mode not implemented");
          }
          CPU_STATE.interrupt_iff1 = false;
          CPU_STATE.interrupt_triggered = false;
        }
      }
    }
    unsafe { CPU_STATE.is_running = false };
  }

  pub fn stop() {
    unsafe { CPU_STATE.is_running = false };
  }

  pub fn trigger_interrupt(db_val : u8) {
    unsafe {
      if !CPU_STATE.interrupt_iff1 {
          // Don't accept interrupts
        return;
      }
      else {
        // Notify the running thread that an interrupt has been triggered
        CPU_STATE.databus_val = db_val;
        CPU_STATE.interrupt_triggered = true;
      }
    }
  }

  pub fn trigger_non_maskable_interrupt(db_val : u8) {
    unsafe {
      if !CPU_STATE.interrupt_iff2 {
          // Don't accept non maskable interrupts
        return;
      }
      else {
        // Notify the running thread that an interrupt has been triggered
        CPU_STATE.databus_val = db_val;
        CPU_STATE.interrupt_triggered = true;
      }
    }
  }

}
