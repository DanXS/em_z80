fn main() {
  let config =
    slint_build::CompilerConfiguration::new()
    .with_style("fluent-light".into());
  slint_build::compile_with_config("./src/ui/main_window.slint", config).unwrap();
}