extern crate mos6502;

use mos6502::cpu::CPU;
use mos6502::memory_map::memory_map::MemMappeable;

struct Ram {
    ram: [u8; 4096]
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            ram: [0; 4096]
        }
    }
}

impl MemMappeable for Ram {
    fn read(&self, address: usize) -> u8 {
        self.ram[address]
    }

    fn slice(&self, from: usize) -> &[u8] {
        &self.ram[from..self.ram.len()]
    }

    fn write(&mut self, address: usize, value: u8) -> u8 {
        self.ram[address as usize] = value;
        value
    }
}

struct Rom {
    rom: Vec<u8>
}

impl Rom {
    pub fn new(rom: Vec<u8>) -> Rom {
        Rom {
            rom: rom
        }
    }
}

impl MemMappeable for Rom {
    fn read(&self, address: usize) -> u8 {
        self.rom[address]
    }

    fn slice(&self, from: usize) -> &[u8] {
        &self.rom[from..self.rom.len()]
    }

    fn write(&mut self, _address: usize, _value: u8) -> u8 {
        panic!("No writes in ROM!")
    }
}

fn main() {
    let ram = Ram::new();

    let program = vec![
        0xa2, 0x01,
        0xa9, 0x05,
        0x85, 0x01,
        0xa9, 0x07,
        0x85, 0x02,
        0xa0, 0x0a,
        0x8c, 0x05, 0x07,
        0xa1, 0x00
    ];

    let rom = Rom::new(program);
    let mut cpu = CPU::new();

    cpu.mount_in_bus(0x0000..0x1000, ram);
    cpu.mount_in_bus(0xe000..0xe010, rom);

    cpu.set_pc(0xe000);

    for _ in 0..8 {
        cpu.step();
    }

    assert_eq!(0x0a, cpu.a);
    assert_eq!(0x01, cpu.x);
    assert_eq!(0x0a, cpu.y);
}
