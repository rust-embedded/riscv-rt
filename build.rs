// NOTE: Adapted from cortex-m/build.rs
use std::env;
use std::fs;
use std::path::PathBuf;

fn main() {
    let target = env::var("TARGET").unwrap();
    let out_dir = PathBuf::from(env::var("OUT_DIR").unwrap());

    if target.starts_with("riscv") {
        let extensions = &target.split('-').next().unwrap()[7..];
        if extensions.contains('m') || extensions.contains('g') {
            println!("cargo:rustc-cfg=riscv_mul");
        }
    }

    // Put the linker script somewhere the linker can find it
    fs::write(out_dir.join("link.x"), include_bytes!("link.x")).unwrap();
    println!("cargo:rustc-link-search={}", out_dir.display());
    println!("cargo:rerun-if-changed=link.x");
}
