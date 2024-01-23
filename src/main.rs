
extern crate em_z80_lib;

use em_z80_lib::*;

fn main() {
    let rom_file_result = load_rom("./roms/spectrum48k.rom");
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    println!("Memory:\n{}", get_memory_ref());
    let mut pc = get_pc();
    while u32::from(pc) <= get_rom_size() {
        let (text, _) = disassemble_addr(pc);
        println!("{:04X?}\t{}", pc, text);
        step();
        pc = get_pc();
        println!("Registers:\n{}", get_reg_ref());
        println!("---------------------------------------");
    }
}
