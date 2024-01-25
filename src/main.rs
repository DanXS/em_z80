
extern crate em_z80_lib;

use em_z80_lib::*;

use std::env;
use std::u16;
use std::rc::Rc;
use slint::{ SharedString, ModelRc, VecModel };

slint::include_modules!();

fn main() {
    let rom_file_result = load_rom("./roms/spectrum48k.rom");
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    let main_window = MainWindow::new().unwrap();
    build_memory_view(&main_window);
    build_register_view(&main_window);
    build_disassembly_view(&main_window);
    main_window.set_is_running(false);
    let weak_window = main_window.as_weak();
    main_window.on_step(move || {
        let (text, _) = disassemble_addr(get_pc());
        println!("Step: {} ", text);
        step();
        build_disassembly_view(&(weak_window.unwrap())); 
        build_register_view(&(weak_window.unwrap()));
        build_memory_view(&(weak_window.unwrap()));
    });
    let weak_window = main_window.as_weak();
    main_window.on_start_stop(move || {
        let is_running = weak_window.unwrap().get_is_running();
        if !is_running {
            println!("Start");
            weak_window.unwrap().set_is_running(!is_running);
        }
        else {
            println!("Stop");
            weak_window.unwrap().set_is_running(!is_running);
        }
    });
    let weak_window = main_window.as_weak();
    main_window.on_set_register(move |reg: SharedString, val: SharedString| {
        let reg_str = reg.as_str();
        let val_str = val.as_str();
        match u16::from_str_radix(val_str, 16) {
            Ok(res) => {
                set_register16(reg_str, res);
                build_register_view(&(weak_window.unwrap()));
                if reg_str == "PC" {
                    build_disassembly_view(&(weak_window.unwrap())); 
                }
            },
            Err(e) => println!("{}", e),
        };
    });
    let weak_window = main_window.as_weak();
    main_window.on_memory_scroll_to_addr(move |val: SharedString| {
        let val_str = val.as_str();
        match u16::from_str_radix(val_str, 16) {
            Ok(res) => {
                let row = res >> 4;
                (weak_window.unwrap()).set_memory_scroll_row(row as i32);
            },
            Err(e) => println!("{}", e),
        };
    });
    main_window.run().unwrap();
    
}

fn build_memory_view(main_window: &MainWindow) {
    let mem_str = get_memory_view_string();
    let str_vec : Vec<&str> = mem_str.split("\n").collect();
    let vec_model : Vec<SharedString> = str_vec.iter().map(|&x| x.into()).collect();
    let memory_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let memory_view_model_rc = ModelRc::from(memory_view_model.clone());
    main_window.set_memory_view_data(memory_view_model_rc);
}

fn build_register_view(main_window: &MainWindow) {
    let reg_str = get_register_view_string();
    let str_vec : Vec<&str> = reg_str.split("\n").collect();
    let vec_model : Vec<SharedString> = str_vec.iter().map(|&x| x.into()).collect();
    let register_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let register_view_model_rc = ModelRc::from(register_view_model.clone());
    main_window.set_register_view_data(register_view_model_rc);
}

fn build_disassembly_view(main_window: &MainWindow) {
    let mut addr = get_pc();
    let mut str_vec : Vec<String> = Vec::new();
    for _ in 0..20 {
        let (text, bytes) = disassemble_addr(addr);
        str_vec.push(format!("{:04X?}:    {}", addr, text));
        if addr as u32 == get_mem_size()-1 {
            break;
        }
        else {
            addr = addr + (bytes as u16);
        }
    }
    let vec_model : Vec<SharedString> = (str_vec.clone()).iter().map(|x| x.into()).collect();
    let disassembly_view_model : Rc<VecModel<SharedString>> = Rc::new(VecModel::from(vec_model));
    let disassembly_view_model_rc = ModelRc::from(disassembly_view_model.clone());
    main_window.set_disassembly_view_data(disassembly_view_model_rc);
}
