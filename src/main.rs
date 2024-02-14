
extern crate em_z80_lib;
extern crate spectrum_lib;

use em_z80_lib::*;
use spectrum_lib::*;
use std::env;
use std::u16;
use std::rc::Rc;
use std::num::NonZeroU32;
use slint::{ SharedString, ModelRc, VecModel, RenderingState };
use glow::HasContext;

slint::include_modules!();

macro_rules! define_scoped_binding {
    (struct $binding_ty_name:ident => $obj_name:path, $param_name:path, $binding_fn:ident, $target_name:path) => {
        struct $binding_ty_name {
            saved_value: Option<$obj_name>,
            gl: Rc<glow::Context>,
        }

        impl $binding_ty_name {
            unsafe fn new(gl: &Rc<glow::Context>, new_binding: Option<$obj_name>) -> Self {
                let saved_value =
                    NonZeroU32::new(gl.get_parameter_i32($param_name) as u32).map($obj_name);

                gl.$binding_fn($target_name, new_binding);
                Self { saved_value, gl: gl.clone() }
            }
        }

        impl Drop for $binding_ty_name {
            fn drop(&mut self) {
                unsafe {
                    self.gl.$binding_fn($target_name, self.saved_value);
                }
            }
        }
    };
    (struct $binding_ty_name:ident => $obj_name:path, $param_name:path, $binding_fn:ident) => {
        struct $binding_ty_name {
            saved_value: Option<$obj_name>,
            gl: Rc<glow::Context>,
        }

        impl $binding_ty_name {
            unsafe fn new(gl: &Rc<glow::Context>, new_binding: Option<$obj_name>) -> Self {
                let saved_value =
                    NonZeroU32::new(gl.get_parameter_i32($param_name) as u32).map($obj_name);

                gl.$binding_fn(new_binding);
                Self { saved_value, gl: gl.clone() }
            }
        }

        impl Drop for $binding_ty_name {
            fn drop(&mut self) {
                unsafe {
                    self.gl.$binding_fn(self.saved_value);
                }
            }
        }
    };
}

define_scoped_binding!(struct ScopedTextureBinding => glow::NativeTexture, glow::TEXTURE_BINDING_2D, bind_texture, glow::TEXTURE_2D);
define_scoped_binding!(struct ScopedFrameBufferBinding => glow::NativeFramebuffer, glow::DRAW_FRAMEBUFFER_BINDING, bind_framebuffer, glow::DRAW_FRAMEBUFFER);
define_scoped_binding!(struct ScopedVBOBinding => glow::NativeBuffer, glow::ARRAY_BUFFER_BINDING, bind_buffer, glow::ARRAY_BUFFER);
define_scoped_binding!(struct ScopedVAOBinding => glow::NativeVertexArray, glow::VERTEX_ARRAY_BINDING, bind_vertex_array);

struct GLTextureFBO {
    texture: glow::Texture,
    width: u32,
    height: u32,
    fbo: glow::Framebuffer,
    gl: Rc<glow::Context>,
}

impl GLTextureFBO {
    unsafe fn new(gl: &Rc<glow::Context>, width: u32, height: u32, pixels: Option<&[u8]>) -> Self {
        let fbo = gl.create_framebuffer().expect("Unable to create framebuffer");

        let texture = gl.create_texture().expect("Unable to allocate texture");

        let _saved_texture_binding = ScopedTextureBinding::new(gl, Some(texture));

        let old_unpack_alignment = gl.get_parameter_i32(glow::UNPACK_ALIGNMENT);
        let old_unpack_row_length = gl.get_parameter_i32(glow::UNPACK_ROW_LENGTH);
        let old_unpack_skip_pixels = gl.get_parameter_i32(glow::UNPACK_SKIP_PIXELS);
        let old_unpack_skip_rows = gl.get_parameter_i32(glow::UNPACK_SKIP_ROWS);

        gl.pixel_store_i32(glow::UNPACK_ALIGNMENT, 1);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MIN_FILTER, glow::LINEAR as i32);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_MAG_FILTER, glow::NEAREST as i32);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_WRAP_S, glow::CLAMP_TO_EDGE as i32);
        gl.tex_parameter_i32(glow::TEXTURE_2D, glow::TEXTURE_WRAP_T, glow::CLAMP_TO_EDGE as i32);
        gl.pixel_store_i32(glow::UNPACK_ROW_LENGTH, width as i32);
        gl.pixel_store_i32(glow::UNPACK_SKIP_PIXELS, 0);
        gl.pixel_store_i32(glow::UNPACK_SKIP_ROWS, 0);

        gl.tex_image_2d(
            glow::TEXTURE_2D,
            0,
            glow::RGBA as _,
            width as _,
            height as _,
            0,
            glow::RGBA as _,
            glow::UNSIGNED_BYTE as _,
            pixels
        );

        let _saved_fbo_binding = ScopedFrameBufferBinding::new(gl, Some(fbo));

        gl.framebuffer_texture_2d(
            glow::FRAMEBUFFER,
            glow::COLOR_ATTACHMENT0,
            glow::TEXTURE_2D,
            Some(texture),
            0,
        );

        debug_assert_eq!(
            gl.check_framebuffer_status(glow::FRAMEBUFFER),
            glow::FRAMEBUFFER_COMPLETE
        );

        gl.pixel_store_i32(glow::UNPACK_ALIGNMENT, old_unpack_alignment);
        gl.pixel_store_i32(glow::UNPACK_ROW_LENGTH, old_unpack_row_length);
        gl.pixel_store_i32(glow::UNPACK_SKIP_PIXELS, old_unpack_skip_pixels);
        gl.pixel_store_i32(glow::UNPACK_SKIP_ROWS, old_unpack_skip_rows);

        Self { texture, width, height, fbo, gl: gl.clone() }
    }

    unsafe fn with_texture_as_active_fbo<R>(&self, callback: impl FnOnce() -> R) -> R {
        let _saved_fbo = ScopedFrameBufferBinding::new(&self.gl, Some(self.fbo));
        callback()
    }
}

impl Drop for GLTextureFBO {
    fn drop(&mut self) {
        unsafe {
            self.gl.delete_framebuffer(self.fbo);
            self.gl.delete_texture(self.texture);
        }
    }
}

struct GLScreenView {
    gl: Rc<glow::Context>,
    program: glow::Program,
    vbo: glow::Buffer,
    vao: glow::VertexArray,
    border_colour_location: glow::UniformLocation,
    displayed_texture: GLTextureFBO,
    next_texture: GLTextureFBO,
    src_texture: GLTextureFBO,
    next_src_texture: GLTextureFBO
}

impl GLScreenView {
    fn new(gl: glow::Context) -> Self {
        let gl = Rc::new(gl);
        unsafe {
            let program = gl.create_program().expect("Cannot create program");

            let (vertex_shader_source, fragment_shader_source) = (
                r#"#version 100
            attribute vec2 a_position;
            varying vec2 frag_position;
            void main() {
                frag_position = a_position;
                gl_Position = vec4(a_position, 0.0, 1.0);
            }"#,
                r#"#version 100
            precision mediump float;
            varying vec2 frag_position;
            uniform vec4 border_colour;
            uniform sampler2D texture;

            void main() {
                vec4 col = border_colour;
                vec2 screen_pos = vec2(160.0*frag_position.x+128.0, (120.0*frag_position.y+96.0));
                if ((screen_pos.x >= 0.0 && screen_pos.x <= 256.0) && 
                    (screen_pos.y >= 0.0 && screen_pos.y <= 192.0)) {
                    vec2 coord = vec2(screen_pos.x/256.0, screen_pos.y/192.0);
                    col = texture2D(texture, coord);
                }
                gl_FragColor = col;
            }"#,
            );

            let shader_sources = [
                (glow::VERTEX_SHADER, vertex_shader_source),
                (glow::FRAGMENT_SHADER, fragment_shader_source),
            ];

            let mut shaders = Vec::with_capacity(shader_sources.len());

            for (shader_type, shader_source) in shader_sources.iter() {
                let shader = gl.create_shader(*shader_type).expect("Cannot create shader");
                gl.shader_source(shader, shader_source);
                gl.compile_shader(shader);
                if !gl.get_shader_compile_status(shader) {
                    panic!("{}", gl.get_shader_info_log(shader));
                }
                gl.attach_shader(program, shader);
                shaders.push(shader);
            }

            gl.link_program(program);
            if !gl.get_program_link_status(program) {
                panic!("{}", gl.get_program_info_log(program));
            }

            for shader in shaders {
                gl.detach_shader(program, shader);
                gl.delete_shader(shader);
            }

            let position_location = gl.get_attrib_location(program, "a_position").unwrap();

            let vbo = gl.create_buffer().expect("Cannot create buffer");
            gl.bind_buffer(glow::ARRAY_BUFFER, Some(vbo));

            let vertices = [-1.0f32, 1.0f32, -1.0f32, -1.0f32, 1.0f32, 1.0f32, 1.0f32, -1.0f32];

            gl.buffer_data_u8_slice(glow::ARRAY_BUFFER, vertices.align_to().1, glow::STATIC_DRAW);

            let vao = gl.create_vertex_array().expect("Cannot create vertex array");
            gl.bind_vertex_array(Some(vao));
            gl.enable_vertex_attrib_array(position_location);
            gl.vertex_attrib_pointer_f32(position_location, 2, glow::FLOAT, false, 8, 0);

            gl.bind_buffer(glow::ARRAY_BUFFER, None);
            gl.bind_vertex_array(None);

            let displayed_texture = GLTextureFBO::new(&gl, 320, 240, None);
            let next_texture = GLTextureFBO::new(&gl, 320, 240, None);

            // Read image buffer from emulated screen memory
            let mut buffer: Vec<u8> = Vec::new();
            read_memory_slice(0x4000, 0x5B00, &mut buffer);
            let byte_buffer:[u8;0x1B00] = buffer.try_into()
                .unwrap_or_else(|v: Vec<u8>| panic!("Expected a Vec of length {} but it was {}", 0x1B00, v.len()));
    
            // Convert ZX Spectrum layout to RGBA image buffer with the ULA
            convert_screen_buffer_rgba(&byte_buffer);

            // Retrieve the converted buffer from the ULA
            let ula_rgb_pixels = get_rgba_buffer();

            // Set up texture with the newly converted RGBA pixel data
            let src_pixel_ref = ula_rgb_pixels.as_ref();
            let src_texture = GLTextureFBO::new(&gl, 256, 192, Some(src_pixel_ref));
            let next_src_texture = GLTextureFBO::new(&gl, 256, 192, Some(src_pixel_ref));

            gl.active_texture(glow::TEXTURE0);
            gl.bind_texture(glow::TEXTURE_2D, Some(src_texture.texture));

            let border_colour_location = gl.get_uniform_location(program, "border_colour").unwrap();

            Self {
                gl,
                program,
                vbo,
                vao,
                border_colour_location,
                displayed_texture,
                next_texture,
                src_texture,
                next_src_texture,
            }
        }
    }
}

impl Drop for GLScreenView {
    fn drop(&mut self) {
        unsafe {
            self.gl.delete_program(self.program);
            self.gl.delete_vertex_array(self.vao);
            self.gl.delete_buffer(self.vbo);
        }
    }
}

impl GLScreenView {
    fn render(&mut self, width: u32, height: u32) -> slint::Image {
        unsafe {
            let gl = &self.gl;

            gl.use_program(Some(self.program));

            let _saved_vbo = ScopedVBOBinding::new(gl, Some(self.vbo));
            let _saved_vao = ScopedVAOBinding::new(gl, Some(self.vao));

            if self.next_texture.width != width || self.next_texture.height != height {
                let mut new_texture = GLTextureFBO::new(gl, width, height, None);
                std::mem::swap(&mut self.next_texture, &mut new_texture); 
            }

            // Display current texture
            gl.active_texture(glow::TEXTURE0);
            gl.bind_texture(glow::TEXTURE_2D, Some(self.src_texture.texture));    

            // Set the border colour
            let border_col = get_border_colour();
            gl.uniform_4_f32(
                Some(&self.border_colour_location),
                border_col[0],
                border_col[1],
                border_col[2],
                border_col[3]);

            // Retrieve the converted buffer from the ULA
            let ula_rgb_pixels = get_rgba_buffer();
            // Set up texture with the newly converted RGBA pixel data
            let src_pixel_ref = ula_rgb_pixels.as_ref();
            // recreate next texture with new pixels
            self.next_src_texture = GLTextureFBO::new(&gl, 256, 192, Some(src_pixel_ref));

            self.next_texture.with_texture_as_active_fbo(|| {
                let mut saved_viewport: [i32; 4] = [0, 0, 0, 0];

                gl.get_parameter_i32_slice(glow::VIEWPORT, &mut saved_viewport);

                gl.viewport(0, 0, self.next_texture.width as _, self.next_texture.height as _);

                gl.draw_arrays(glow::TRIANGLE_STRIP, 0, 4);

                gl.viewport(
                    saved_viewport[0],
                    saved_viewport[1],
                    saved_viewport[2],
                    saved_viewport[3],
                );
            });

            gl.use_program(None);

        }

        let result_texture = unsafe {
            slint::BorrowedOpenGLTextureBuilder::new_gl_2d_rgba_texture(
                self.next_texture.texture.0,
                (self.next_texture.width, self.next_texture.height).into(),
            )
            .build()
        };

        std::mem::swap(&mut self.next_texture, &mut self.displayed_texture);
        std::mem::swap(&mut self.next_src_texture, &mut self.src_texture);

        result_texture
    }
}


fn main() {

    // Load a spectrum 48k rom image
    let rom_file_result = load_bin("./roms/spectrum48k.rom", 0x0000);
    match rom_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem reading the file: {:?}", error),
    };
    // Load a screenshot for testing display without running instuctions
    /* 
    let screenshot_file_result = load_bin("./screenshots/Jetpack.scr", 0x4000);
    match screenshot_file_result {
        Ok(file) => file,
        Err(error) => panic!("Problem reading the file: {:?}", error),
    };
    */
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
    main_window.on_keyboard_key_pressed(move |text : SharedString, control : bool, meta: bool, shift : bool | {
        let key = text.as_str();
        // Meta and control or'ed together so either can mean symbol so works with command key on mac
        // Note: some command keys may be mapped to menu, for example command h will hide window
        // so better to use control keys (on mac), strangely is says meta is for command key, but seems to
        // be control key on my mac?
        key_down_event(key, shift, control || meta);
    });
    main_window.on_keyboard_key_released(move |text : SharedString, control : bool, meta: bool, shift : bool | {
        let key = text.as_str();
        // Meta and control or'ed together so either can mean symbol so works with command key on mac
        key_up_event(key, shift,  control || meta);
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
    // Thread to manage vertical blanking gap interrupts from the ULA
    std::thread::spawn(move || {
        loop {
            // Wait for next vertical blanking gap
            let data = next_vblank();
            // Read the memory display memory
            let mut buffer: Vec<u8> = Vec::new();
            read_memory_slice(0x4000, 0x5B00, &mut buffer);
            let byte_buffer:[u8;0x1B00] = buffer.try_into()
                .unwrap_or_else(|v: Vec<u8>| panic!("Expected a Vec of length {} but it was {}", 0x1B00, v.len()));
            // Convert the RGBA
            convert_screen_buffer_rgba(&byte_buffer);
            // Trigger vertical blanking gap interrupt
            trigger_interrupt(data);
        }
    });
    // Set up OpenGLES rendering for main display of emulated screen
    let mut screenview = None;
    let weak_window = main_window.as_weak();
    if let Err(error) = main_window.window().set_rendering_notifier(move |state, graphics_api| {
        match state {
            RenderingState::RenderingSetup => {
                let context = match graphics_api {
                    #[cfg(not(target_arch = "wasm32"))]
                    slint::GraphicsAPI::NativeOpenGL { get_proc_address } => unsafe {
                        glow::Context::from_loader_function_cstr(|s| get_proc_address(s))
                    },
                    #[cfg(target_arch = "wasm32")]
                    slint::GraphicsAPI::WebGL { canvas_element_id, context_type } => {
                        use wasm_bindgen::JsCast;

                        let canvas = web_sys::window()
                            .unwrap()
                            .document()
                            .unwrap()
                            .get_element_by_id(canvas_element_id)
                            .unwrap()
                            .dyn_into::<web_sys::HtmlCanvasElement>()
                            .unwrap();

                        let webgl1_context = canvas
                            .get_context(context_type)
                            .unwrap()
                            .unwrap()
                            .dyn_into::<web_sys::WebGlRenderingContext>()
                            .unwrap();

                        glow::Context::from_webgl1_context(webgl1_context)
                    }
                    _ => return,
                };
                screenview = Some(GLScreenView::new(context))
            },
            RenderingState::BeforeRendering => {
                if let (Some(screenview), Some(app)) = (screenview.as_mut(), weak_window.upgrade()) {
                    let texture = screenview.render(
                        app.get_requested_texture_width() as u32,
                        app.get_requested_texture_height() as u32,
                    );
                    app.set_texture(slint::Image::from(texture));  
                    app.window().request_redraw();
                }
            },
            RenderingState::AfterRendering => {
                
            },
            RenderingState::RenderingTeardown => {
                println!("Rendering Teardown");
            },
            _ => {}
        }
    }) {
        match error {
            slint::SetRenderingNotifierError::Unsupported => eprintln!("This example requires the use of the GL backend. Please run with the environment variable SLINT_BACKEND=GL set."),
            _ => unreachable!()
        }
        std::process::exit(1);
    }
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

