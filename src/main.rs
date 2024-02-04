
extern crate em_z80_lib;
extern crate spectrum_lib;

use em_z80_lib::*;
use spectrum_lib::*;
use std::env;
use std::u16;
use std::rc::Rc;
use slint::{ SharedString, ModelRc, VecModel };

slint::include_modules!();

fn main() {
    hello_ula();
    let rom_file_result = load_rom("./roms/spectrum48k.rom");
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem opening the file: {:?}", error),
    };
    set_cpu_frequency(3.5f32);
    let main_window = MainWindow::new().unwrap();
    build_memory_view(&main_window);
    build_register_view(&main_window);
    build_status_register_flag_view(&main_window);
    build_disassembly_view(&main_window);
    main_window.set_is_running(false);
    update_breakpoints_enabled(true);
    main_window.set_enable_breakpoints(true);
    let weak_window = main_window.as_weak();
    main_window.on_step(move || {
        let (text, _) = disassemble_addr(get_pc());
        println!("Step:\n{:04X?}: {} ", get_pc(), text);
        step();
        build_disassembly_view(&(weak_window.unwrap())); 
        build_register_view(&(weak_window.unwrap()));
        build_status_register_flag_view(&(weak_window.unwrap()));
        build_memory_view(&(weak_window.unwrap()));
    });
    let weak_window = main_window.as_weak();
    main_window.on_start_stop(move || {
        let is_running = weak_window.unwrap().get_is_running();
        if !is_running {
            weak_window.unwrap().set_is_running(!is_running);
            let window_copy = weak_window.clone();
            std::thread::spawn(move || {
                run();
                let _ = slint::invoke_from_event_loop(move || {
                    window_copy.unwrap().set_is_running(false);
                    build_disassembly_view(&(window_copy.unwrap())); 
                    build_register_view(&(window_copy.unwrap()));
                    build_status_register_flag_view(&(window_copy.unwrap()));
                    build_memory_view(&(window_copy.unwrap()));
                });
            });
        }
        else {
            weak_window.unwrap().set_is_running(!is_running);
            stop();
        }
    });
    let weak_window = main_window.as_weak();
    main_window.on_set_register(move |reg: SharedString, val: SharedString| {
        let reg_str = reg.as_str(); 
        let val_str = val.as_str();
        match u16::from_str_radix(val_str, 16) {
            Ok(res) => {
                set_register16(get_register_from_str(reg_str), res);
                build_register_view(&(weak_window.unwrap()));
                if reg_str == "PC" {
                    build_disassembly_view(&(weak_window.unwrap())); 
                }
                else if reg_str == "AF" {
                    build_status_register_flag_view(&(weak_window.unwrap()))
                }
            },
            Err(e) => println!("{}", e),
        };
    });
    let weak_window = main_window.as_weak();
    main_window.on_update_status_flag(move |flag: SharedString, val: bool| {
        let flag_str = flag.as_str();
        if val {
            set_status_flag(flag_str);
        }
        else {
            clear_status_flag(flag_str);
        }
        build_register_view(&(weak_window.unwrap()));
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
    main_window.on_toggle_breakpoint(move |line: SharedString| {
        let line_str = line.as_str();
        match address_from_disassembly(line_str) {
            Ok(res) => {
                toggle_breakpoint(res);
            },
            Err(e) => println!("{}", e),
        };
    });
    let weak_window = main_window.as_weak();
    main_window.on_toggle_enable_breakpoints(move || {
        let is_enabled = weak_window.unwrap().get_enable_breakpoints();
        (weak_window.unwrap()).set_enable_breakpoints(!is_enabled);
        update_breakpoints_enabled(!is_enabled);
        build_disassembly_view(&(weak_window.unwrap()));
    });
    main_window.global::<Logic>().on_has_breakpoint(|line| {
        let line_str = line.as_str();
        match address_from_disassembly(line_str) {
            Ok(res) => {
                has_breakpoint(res)
            },
            _ => false,
        }
    });
    main_window.run().unwrap();
}

pub fn address_from_disassembly(line: &str) -> Result<u16, std::num::ParseIntError> {
    let addr_str = line.split_once(":").unwrap().0;
    u16::from_str_radix(&addr_str, 16)
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

fn build_status_register_flag_view(main_window: &MainWindow) {
    let vec_model: Vec<bool> = [
        get_status_flag("C"),
        get_status_flag("N"),
        get_status_flag("P/V"),
        get_status_flag("H"),
        get_status_flag("Z"),
        get_status_flag("S")
    ].to_vec();
    println!("status flags: C:{} N:{} P/V:{} H:{} Z:{} S:{}",
        vec_model[0], vec_model[1], vec_model[2], vec_model[3], vec_model[4], vec_model[5]);
    let status_view_model : Rc<VecModel<bool>> = Rc::new(VecModel::from(vec_model));
    let status_view_model_rc = ModelRc::from(status_view_model.clone());
    main_window.set_status_view_data(status_view_model_rc);
}

fn build_disassembly_view(main_window: &MainWindow) {
    let mut addr = get_pc();
    let mut str_vec : Vec<String> = Vec::new();
    for _ in 0..1000 {
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

