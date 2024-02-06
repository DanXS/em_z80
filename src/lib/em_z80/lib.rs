mod util;
mod cpu;
mod instructions;
mod registers;
mod memory;
mod opcodes;

extern crate core;

use std::io;
use registers::Flag;
use registers::RegID;
use crate::cpu::Cpu;
use crate::memory::Memory;

// Get the size of the memory used for the emulation
pub fn get_mem_size() -> u32 {
  memory::MEM_SIZE
}

// Get the size of the rom area of memory
pub fn get_rom_size() -> u32 {
  memory::ROM_SIZE
}

// Load a binary image file into memory starting at a given start address
pub fn load_bin(filename: &str, start_addr: usize) -> io::Result<()> {
  Memory::load_bin(filename, start_addr)
}

// Get a reference to the memory
pub fn get_memory_ref() -> &'static Memory {
  &Memory
}

// Get a string representing entire memory view
pub fn get_memory_view_string() -> String {
  Memory.to_string()
}

// Set the CPU's frequency to emulate at. Defaults to 3.5Mhz
pub fn set_cpu_frequency(mhz: f32) {
  Cpu::set_cpu_frequency(mhz);
}

// Get the program counter's current address
pub fn get_pc() -> u16 {
  Cpu::get_pc()
}

// Set the program counter to a particular address
pub fn set_pc(addr: u16) {
  Cpu::set_pc(addr);
}

// Get a 8 bit register value
pub fn get_register8(reg: RegID) -> u8 {
  Cpu::get_register8(reg)
}

// Set a 8 bit register to a specified value
pub fn set_register8(reg: RegID, val: u8) {
  Cpu::set_register8(reg, val);
}

// Get a 16 bit register value
pub fn get_register16(reg: RegID) -> u16 {
  Cpu::get_register16(reg)
}

// Set a 16 bit register to a specified value
pub fn set_register16(reg: RegID, val: u16) {
  Cpu::set_register16(reg, val);
}

pub fn get_register_from_str(reg_str: &str) -> RegID {
  Cpu::get_register_from_str(reg_str)
}

// Set the status flag
// Flag names should be one of "C", "N", "P/V", "H", "Z" or "S"
pub fn set_status_flag(flag: &str) {
  match flag {
    "C" => Cpu::set_status_flag(Flag::C),
    "N" => Cpu::set_status_flag(Flag::N),
    "P/V" => Cpu::set_status_flag(Flag::PV),
    "H" => Cpu::set_status_flag(Flag::H),
    "Z" => Cpu::set_status_flag(Flag::Z),
    "S" => Cpu::set_status_flag(Flag::S),
    _ => panic!("Status flag does not exist!")
  }
}

// Clear the status flag
// Flag names should be one of "C", "N", "P/V", "H", "Z" or "S"
pub fn clear_status_flag(flag: &str) {
  match flag {
    "C" => Cpu::clear_status_flag(Flag::C),
    "N" => Cpu::clear_status_flag(Flag::N),
    "P/V" => Cpu::clear_status_flag(Flag::PV),
    "H" => Cpu::clear_status_flag(Flag::H),
    "Z" => Cpu::clear_status_flag(Flag::Z),
    "S" => Cpu::clear_status_flag(Flag::S),
    _ => panic!("Status flag does not exist!")
  }
}

// Get the status flag
// Flag names should be one of "C", "N", "P/V", "H", "Z" or "S"
pub fn get_status_flag(flag: &str) -> bool {
  match flag {
    "C" => Cpu::get_status_flag(Flag::C),
    "N" => Cpu::get_status_flag(Flag::N),
    "P/V" => Cpu::get_status_flag(Flag::PV),
    "H" => Cpu::get_status_flag(Flag::H),
    "Z" => Cpu::get_status_flag(Flag::Z),
    "S" => Cpu::get_status_flag(Flag::S),
    _ => panic!("Status flag does not exist!")
  }
}

// Step a single instruction at the current address given by the program counter
pub fn step() {
  Cpu::step();
}

// Run continiously until a breakpoint is encountered (if enabled) or stop is called
// Note: should be called asynchronously
pub fn run() {
  Cpu::run();
}

// Stop the running task
pub fn stop() {
  Cpu::stop();
}

// Get a string representing registers
pub fn get_register_view_string() -> String {
  Cpu::get_register_view_string()
}

// Dissassmble a given address, returns the dissabled string and the number of bytes
// used to encode the instruction
pub fn disassemble_addr(addr: u16) -> (String, u8) {
  let result = Cpu::disassemble(addr);
  return result;
}

// Toggle a breakpoint at a given address, if a breakpoint already exists it will be removed
// otherwise it will be added
pub fn toggle_breakpoint(addr: u16) {
  Cpu::toggle_breakpoint(addr);
}

// Returns true if a breakpoint exists at a given address, otherwise false
pub fn has_breakpoint(addr: u16) -> bool {
  Cpu::has_breakpoint(addr)
}

// Enable or disable breakpoints
pub fn update_breakpoints_enabled(enabled: bool) {
  Cpu::update_breakpoints_enabled(enabled);
}

// Trigger an interrupt with a given value on the databus
// Note: This would normally be done by the ULA every verticle blanking gap
// in the case of the ZX Spectrum
pub fn trigger_interrupt(db_val : u8) {
  Cpu::trigger_interrupt(db_val);
}

// Trigger non maskable interrupts
// Note: This would normally be done by external peripherals plugged into the expansion 
// port in the case of the ZX Spectrum
pub fn trigger_non_maskable_interrupt(db_val : u8) {
  Cpu::trigger_non_maskable_interrupt(db_val);
}

pub fn read_memory_slice(start_addr: usize, end_addr: usize, buffer: &mut Vec<u8>) {
  Memory::read_slice(start_addr, end_addr, buffer);
}
