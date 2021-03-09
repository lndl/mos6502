pub mod memory_map {
    use std::fmt;
    use std::ops::Range;
    use std::collections::HashMap;
    use std::rc::Rc;
    use std::cell::RefCell;

    use instruction::Instruction;

    pub const STKBASE : u16 = 0x100;
    pub const NMIVECADR : u16 = 0xfffa;
    pub const RESETVECADR : u16 = 0xfffc;
    pub const INTVECADR : u16 = 0xfffe;

    pub trait Memorable {
        fn read(&self, address: usize) -> u8;
        fn write(&mut self, address: usize, value: u8) -> u8;
        fn slice(&self, from: usize) -> &[u8];
    }

    pub struct MemoryMap {
        devices: HashMap<Range<usize>, Rc<RefCell<dyn Memorable>>>,
    }

    impl MemoryMap {
        pub fn new() -> MemoryMap {
            MemoryMap {
                devices: HashMap::new(),
            }
        }

        pub fn register_device(&mut self, mem_range: Range<usize>, device: Rc<RefCell<dyn Memorable>>) {
            self.devices.insert(mem_range, device);
        }

        pub fn push_to_stack(&mut self, sp: u8, value: u8) -> u8 {
            self.write(STKBASE + sp as u16, value);
            value
        }

        pub fn peek_from_stack(&self, sp: u8) -> u8 {
            self.read(STKBASE + sp as u16).unwrap_or(0)
        }

        pub fn read(&self, address: u16) -> Option<u8> {
            let address = address as usize;
            for (mem_range, reader) in &self.devices {
                if mem_range.contains(&address) {
                    let base = mem_range.start;
                    return Some(reader.borrow().read(address - base))
                }
            }
            None
        }

        pub fn write(&mut self, address: u16, value: u8) -> u8 {
            let address = address as usize;
            for (mem_range, writer) in &mut self.devices {
                if mem_range.contains(&address) {
                    let base = mem_range.start;
                    return writer.borrow_mut().write(address - base, value)
                }
            }
            panic!("Invalid write to 0x{:04x}", address)
        }

        //FIXME: Improve this
        pub fn fetch_instruction(&self, address: u16) -> Option<Instruction> {
            let address = address as usize;
            for (mem_range, reader) in &self.devices {
                if mem_range.contains(&address) {
                    let base = mem_range.start;
                    return Instruction::build(reader.borrow().slice(address - base));
                }
            }
            panic!("Invalid access from 0x{:04x}", address)
        }
    }

    impl fmt::Debug for MemoryMap {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "================= Memory Dump: =======================\n")?;
            for byte in 0x0000..=0xFFFF {
                if byte % 0x10 == 0 {
                    write!(f, "{:04x}: ", byte);
                }
                match self.read(byte) {
                    Some(v) => {
                        write!(f, "{:02x} ", v);
                    },
                    None => {
                        write!(f, "-- ");
                    }
                }
            }
            write!(f, "======================================================\n")
        }
    }
}