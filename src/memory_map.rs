pub mod memory_map {
    use std::fmt;
    use std::ops::Range;
    use std::collections::HashMap;
    use std::cell::RefCell;

    use instruction::Instruction;

    pub const STKBASE : u16 = 0x100;
    pub const INTVECADR : u16 = 0xfffe;
    pub const RESETVECADR : u16 = 0xfffc;

    pub trait MemMappeable {
        fn read(&self, address: usize) -> u8;
        fn slice(&self, from: usize) -> &[u8];
        fn write(&mut self, address: usize, value: u8) -> u8;
    }

    pub struct MemoryMap {
        devices: HashMap<Range<usize>, RefCell<Box<dyn MemMappeable>>>,
    }

    impl MemoryMap {
        pub fn new() -> MemoryMap {
            MemoryMap {
                devices: HashMap::new(),
            }
        }

        pub fn register_device(&mut self, mem_range: Range<usize>, device: impl MemMappeable + 'static) {
            self.devices.insert(mem_range, RefCell::new(Box::new(device)));
        }

        pub fn push_to_stack(&mut self, sp: u8, value: u8) -> u8 {
            self.write(STKBASE + sp as u16, value);
            value
        }

        pub fn peek_from_stack(&self, sp: u8) -> u8 {
            self.read(STKBASE + sp as u16)
        }

        pub fn read(&self, address: u16) -> u8 {
            self.device_for(address).borrow().read(self.address_inside_device(address))
        }

        pub fn write(&mut self, address: u16, value: u8) -> u8 {
            self.device_for(address).borrow_mut().write(self.address_inside_device(address), value)
        }

        pub fn fetch_instruction(&self, address: u16) -> Option<Instruction> {
            Instruction::build(self.device_for(address).borrow().slice(self.address_inside_device(address)))
        }

        pub fn device_for(&self, address: u16) -> &RefCell<Box<dyn MemMappeable>> {
            let address = address as usize;
            for (mem_range, device) in &self.devices {
                if mem_range.contains(&address) {
                    return device
                }
            }
            panic!("Invalid access from 0x{:04x}", address)
        }

        fn address_inside_device(&self, address: u16) -> usize {
            let address = address as usize;
            for (mem_range, _) in &self.devices {
                if mem_range.contains(&address) {
                    return address - mem_range.start
                }
            }
            panic!("Invalid translation from requested address 0x{:04x}", address)
        }

    }

    impl fmt::Debug for MemoryMap {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "Memory Dump:\n")?;
            write!(f, "================================================================================\n")?;
            let mut mem_ranges : Vec<_> = self.devices.keys().clone().into_iter().collect();
            mem_ranges.sort_by(|r1, r2| r1.start.cmp(&r2.start));
            for mem_range in mem_ranges  {
                let mem_range : Vec<_> = mem_range.clone().collect();
                for mem_chunk in mem_range.chunks(16) {
                    let line : Vec<std::string::String> = mem_chunk.into_iter()
                        .map(|x| format!("{:02x}", self.read(*x as u16))).collect();
                    write!(f, "{:04x}: {}\n", mem_chunk[0], line.join(" "))?;
                }
                write!(f, "<<<<<<<<<<<<<< SKIP >>>>>>>>>>>>>>\n")?;
            }
            write!(f, "================================================================================\n")
        }
    }
}