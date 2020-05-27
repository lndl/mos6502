use std::fmt;
use std::collections::HashSet;
use std::ops::Range;

use instruction::AddressingMode;
use memory_map::*;

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
enum Flag {
    Carry,
    Zero,
    IntDisabled,
    Decimal,
    Break,
    Overflow,
    Negative,
}

impl Flag {
    fn values() -> Vec<Flag> {
        //TODO: Missing BREAK
        vec![ Self::Carry, Self::Zero, Self::IntDisabled,
        Self::Decimal, Self::Overflow, Self::Negative ]
    }
}

struct CPUFlags {
    flags: HashSet<Flag>
}

impl CPUFlags {
    fn new() -> CPUFlags {
        CPUFlags { flags: HashSet::new() }
    }

    fn bit(&self, f: Flag) -> u8 {
        if self.flags.contains(&f) { 1 } else { 0 }
    }

    fn has_set(&self, f: Flag) -> bool {
        self.flags.contains(&f)
    }

    fn set(&mut self, f: Flag, val: bool) {
        if val {
            self.flags.insert(f);
        } else {
            self.flags.remove(&f);
        }
    }

    fn to_byte(&self) -> u8 {
        let mut flagsbyte = 0x20;
        let mut index     = 1;
        let mask          = 0b11001111;
        for f in Flag::values() {
            while (mask & index) != index {
                index <<= 1;
            }
            if self.has_set(f) {
                flagsbyte += index;
            }
            index <<= 1;
        }
        flagsbyte
    }

    fn from_byte(&mut self, flagsbyte: u8) {
        let mut index = 1;
        let mask      = 0b11001111;
        for f in Flag::values() {
            while (mask & index) != index {
                index <<= 1;
            }
            self.set(f, (flagsbyte & index) == index);
            index <<= 1;
        }
    }
}

impl fmt::Debug for CPUFlags {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Flag::*;

        write!(f, "{}", if self.has_set(Negative)    { "N" } else { "-" })?;
        write!(f, "{}", if self.has_set(Overflow)    { "V" } else { "-" })?;
        write!(f, "_")?;
        write!(f, "{}", if self.has_set(Break)       { "B" } else { "-" })?;
        write!(f, "{}", if self.has_set(Decimal)     { "D" } else { "-" })?;
        write!(f, "{}", if self.has_set(IntDisabled) { "I" } else { "-" })?;
        write!(f, "{}", if self.has_set(Zero)        { "Z" } else { "-" })?;
        write!(f, "{}", if self.has_set(Carry)       { "C" } else { "-" })?;
        write!(f, " [{:02x}]", self.to_byte())
    }
}

pub struct CPU {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    sp: u8,
    pc: u16,

    flags: CPUFlags,

    mem: memory_map::MemoryMap,

    jmp_bug: bool,
    decimal_mode_enabled: bool
}

impl CPU {
    pub fn new() -> Self {
        let flags = CPUFlags::new();
        let mem = memory_map::MemoryMap::new();

        CPU {
            a: 0,
            x: 0,
            y: 0,
            sp: 0xFD,
            pc: 0,
            flags,
            mem,
            jmp_bug: true,
            decimal_mode_enabled: false
        }
    }

    pub fn reset(&mut self) {
        self.pc = self.fetch_address_from_vector(memory_map::RESETVECADR);
        self.a = 0;
        self.x = 0;
        self.y = 0;
        self.sp = 0xFD;
        // TODO: Implement!
        // self.flags.reset();
        // self.mem.reset();
    }

    pub fn mount_in_bus(&mut self, mem_range: Range<usize>, device: impl memory_map::MemMappeable + 'static) {
        self.mem.register_device(mem_range, device);
    }

    pub fn exec(&mut self, from_address: Option<u16>) {
        match from_address {
            Some(address) => self.pc = address,
                     None => self.reset()
        }
        loop { self.step() }
    }

    pub fn step(&mut self) {
      match self.mem.fetch_instruction(self.pc) {
          Some(instruction) => {
              println!("{:04x}: {:?} | {:?}", self.pc, instruction, self);
              self.pc = self.pc.wrapping_add(instruction.bytesize() as u16);
              instruction.exec(self);
          },
          None => {
              println!("{:04x}: Unknown instruction fetched: {:02x}", self.pc, self.mem.read(self.pc));
              self.pc += 1; // Acts as NOP
          }
      };
    }

    pub fn set_pc(&mut self, address: u16) {
        self.pc = address;
    }

    pub fn adc(&mut self, am: &AddressingMode) {
        let reg_a = self.a as u16;
        let op    = self.read_op(am) as u16;
        let carry = self.flags.has_set(Flag::Carry) as u16;
        let result = reg_a.wrapping_add(op).wrapping_add(carry);

        if self.decimal_mode_enabled && self.flags.has_set(Flag::Decimal) {
            let mut tmp_a = (reg_a & 0x0f).wrapping_add(op & 0xf).wrapping_add(carry);
            if tmp_a >  0x9  {
                tmp_a = tmp_a.wrapping_add(0x6);
            }
            if tmp_a <= 0x0f {
                tmp_a = (tmp_a & 0xf).wrapping_add(reg_a & 0xf0).wrapping_add(op & 0xf0);
            } else {
                tmp_a = (tmp_a & 0xf).wrapping_add(reg_a & 0xf0).wrapping_add(op & 0xf0).wrapping_add(0x10);
            }
            if (tmp_a & 0x1f0) > 0x90 {
                tmp_a = tmp_a.wrapping_add(0x60);
            }
            self.flags.set(Flag::Carry, (tmp_a & 0xff0) > 0xf0);
            self.flags.set(Flag::Overflow, (((reg_a ^ op) & 0x80) != 0) && (((reg_a ^ result) & 0x80) != 0));
            self.a = tmp_a as u8;
        } else {
            self.flags.set(Flag::Carry, result > 0xFF);
            self.flags.set(Flag::Overflow, !(((reg_a ^ op) & 0x80) != 0) && (((reg_a ^ result) & 0x80) != 0));
            self.a = result as u8;
        }
        self.check_nf_with(result as u8);
        self.check_zf_with(result as u8);
    }

    pub fn and(&mut self, am: &AddressingMode) {
        let m = self.read_op(am);
        self.a = self.a & m;
        self.check_zf_with(self.a);
        self.check_nf_with(self.a);
    }

    pub fn asl(&mut self, am: &AddressingMode) {
        let mut o = self.read_op(am);
        self.flags.set(Flag::Carry, (o & 0x80) == 0x80);
        o <<= 1;
        self.write_op(am, o);
        self.check_nf_with(o);
        self.check_zf_with(o);
    }

    pub fn bcc(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Carry, false);
    }

    pub fn bcs(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Carry, true);
    }

    pub fn beq(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Zero, true);
    }

    pub fn bit(&mut self, am: &AddressingMode) {
        let m = self.read_op(am);
        self.flags.set(Flag::Zero, (self.a & m) == 0);
        self.flags.set(Flag::Negative, (m & 0x80) == 0x80);
        self.flags.set(Flag::Overflow, (m & 0x40) == 0x40);
    }

    pub fn bmi(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Negative, true);
    }

    pub fn bne(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Zero, false);
    }

    pub fn bpl(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Negative, false);
    }

    pub fn brk(&mut self, _am: &AddressingMode) {
        // Push PC
        self.push_st_16(self.pc);
        // Push Flags
        self.push_st_8(self.flags.to_byte());
        // Set PC to Interruption Vector address
        self.pc = self.fetch_address_from_vector(memory_map::INTVECADR);
        // Set Flags
        self.flags.set(Flag::IntDisabled, true);
        self.flags.set(Flag::Break, true);
    }

    pub fn bvc(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Overflow, false);
    }

    pub fn bvs(&mut self, am: &AddressingMode) {
        self.branch_on_flag(am, Flag::Overflow, true);
    }

    pub fn clc(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::Carry, false);
    }

    pub fn cld(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::Decimal, false);
    }

    pub fn cli(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::IntDisabled, false);
    }

    pub fn clv(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::Overflow, false);
    }

    pub fn cmp(&mut self, am: &AddressingMode) {
        self.compare(am, self.a);
    }

    pub fn cpx(&mut self, am: &AddressingMode) {
        self.compare(am, self.x);
    }

    pub fn cpy(&mut self, am: &AddressingMode) {
        self.compare(am, self.y);
    }

    pub fn dec(&mut self, am: &AddressingMode) {
        let r = self.read_op(am).wrapping_sub(1);
        self.write_op(am, r);
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn dex(&mut self, _am: &AddressingMode) {
        let r = self.x.wrapping_sub(1);
        self.x = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn dey(&mut self, _am: &AddressingMode) {
        let r = self.y.wrapping_sub(1);
        self.y = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn eor(&mut self, am: &AddressingMode) {
        let r = self.a ^ self.read_op(am);
        self.a = r;
        self.check_zf_with(r);
        self.check_nf_with(r);
    }

    pub fn inc(&mut self, am: &AddressingMode) {
        let r = self.read_op(am).wrapping_add(1);
        self.write_op(am, r);
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn inx(&mut self, am: &AddressingMode) {
        let r = self.read_op(am).wrapping_add(1);
        self.x = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn iny(&mut self, am: &AddressingMode) {
        let r = self.read_op(am).wrapping_add(1);
        self.y = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn jmp(&mut self, am: &AddressingMode) {
        let new_pc = self.resolve_mem_address(am);
        self.pc = new_pc;
    }

    pub fn jsr(&mut self, am: &AddressingMode) {
        let ret_at = self.pc - 1;
        self.push_st_16(ret_at);
        self.pc = self.resolve_mem_address(am);
    }

    pub fn lda(&mut self, am: &AddressingMode) {
        let r = self.read_op(am);
        self.a = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn ldx(&mut self, am: &AddressingMode) {
        let r = self.read_op(am);
        self.x = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn ldy(&mut self, am: &AddressingMode) {
        let r = self.read_op(am);
        self.y = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn lsr(&mut self, am: &AddressingMode) {
        let op = self.read_op(am);
        let r =  op >> 1;
        self.write_op(am, r);
        self.flags.set(Flag::Carry, (op & 0x01) == 0x01);
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn nop(&mut self, _am: &AddressingMode) {
        // Do nothing
    }

    pub fn ora(&mut self, am: &AddressingMode) {
        let r = self.a | self.read_op(am);
        self.a = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn pha(&mut self, _am: &AddressingMode) {
        self.push_st_8(self.a);
    }

    pub fn php(&mut self, _am: &AddressingMode) {
        self.push_st_8(self.flags.to_byte());
    }

    pub fn pla(&mut self, _am: &AddressingMode) {
        let r = self.pop_st_8();
        self.a = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn plp(&mut self, _am: &AddressingMode) {
        let flagsbyte = self.pop_st_8();
        self.flags.from_byte(flagsbyte);
    }

    pub fn rol(&mut self, am: &AddressingMode) {
        let op = self.read_op(am);
        let has_new_carry = (op & 0x80) == 0x80;
        let mut r =  op << 1;
        r = r | self.flags.bit(Flag::Carry);
        self.write_op(am, r);
        self.flags.set(Flag::Carry, has_new_carry);
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn ror(&mut self, am: &AddressingMode) {
        let op = self.read_op(am);
        let has_new_carry = (op & 0x01) == 0x01;
        let mut r =  op >> 1;
        r = r | (self.flags.bit(Flag::Carry) << 7);
        self.write_op(am, r);
        self.flags.set(Flag::Carry, has_new_carry);
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn rti(&mut self, _am: &AddressingMode) {
        // Retrive Program Flags
        let flagsbyte = self.pop_st_8();
        self.flags.from_byte(flagsbyte);
        // Retrieve PC
        self.pc = self.pop_st_16();
    }

    pub fn rts(&mut self, _am: &AddressingMode) {
        let new_pc = self.pop_st_16() + 1;
        self.pc = new_pc;
    }

    pub fn sbc(&mut self, am: &AddressingMode) {
        let reg_a = self.a as u16;
        let op    = self.read_op(am) as u16;
        let carry = !self.flags.has_set(Flag::Carry) as u16;
        let result = reg_a.wrapping_sub(op).wrapping_sub(carry);

        if self.decimal_mode_enabled && self.flags.has_set(Flag::Decimal) {
            let mut tmp_a = (reg_a & 0xf).wrapping_sub(op & 0xf).wrapping_sub(carry);
            if (tmp_a & 0x10) != 0 {
                tmp_a = ((tmp_a.wrapping_sub(6)) & 0xf) | ((reg_a & 0xf0).wrapping_sub(op & 0xf0).wrapping_sub(0x10));
            } else {
                tmp_a = (tmp_a & 0xf) | (reg_a & 0xf0).wrapping_sub(op & 0xf0);
            }
            if (tmp_a & 0x100) != 0 {
                tmp_a = tmp_a.wrapping_sub( 0x60 );
            }
            self.a = tmp_a as u8;
        } else {
            self.a = result as u8;
        }
        self.check_nf_with(result as u8);
        self.check_zf_with(result as u8);
        self.flags.set(Flag::Overflow, (((reg_a ^ op) & 0x80) != 0) && (((reg_a ^ result) & 0x80) != 0));
        self.flags.set(Flag::Carry, result < 0x100);
    }

    pub fn sec(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::Carry, true);
    }

    pub fn sed(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::Decimal, true);
    }

    pub fn sei(&mut self, _am: &AddressingMode) {
        self.flags.set(Flag::IntDisabled, true);
    }

    pub fn sta(&mut self, am: &AddressingMode) {
        self.write_op(am, self.a);
    }

    pub fn stx(&mut self, am: &AddressingMode) {
        self.write_op(am, self.x);
    }

    pub fn sty(&mut self, am: &AddressingMode) {
        self.write_op(am, self.y);
    }

    pub fn tax(&mut self, _am: &AddressingMode) {
        self.x = self.a;
        self.check_nf_with(self.x);
        self.check_zf_with(self.x);
    }

    pub fn tay(&mut self, _am: &AddressingMode) {
        self.y = self.a;
        self.check_nf_with(self.y);
        self.check_zf_with(self.y);
    }

    pub fn tsx(&mut self, _am: &AddressingMode) {
        self.x = self.sp;
        self.check_nf_with(self.x);
        self.check_zf_with(self.x);
    }

    pub fn txa(&mut self, _am: &AddressingMode) {
        self.a = self.x;
        self.check_nf_with(self.a);
        self.check_zf_with(self.a);
    }

    pub fn txs(&mut self, _am: &AddressingMode) {
        self.sp = self.x;
    }

    pub fn tya(&mut self, _am: &AddressingMode) {
        self.a = self.y;
        self.check_nf_with(self.a);
        self.check_zf_with(self.a);
    }

    // Unofficial instructions

    pub fn dcp(&mut self, am: &AddressingMode) {
        self.dec(am);
        self.cmp(am);
    }

    pub fn isb(&mut self, am: &AddressingMode) {
        self.inc(am);
        self.sbc(am);
    }

    pub fn lax(&mut self, am: &AddressingMode) {
        let r = self.read_op(am);
        self.a = r;
        self.x = r;
        self.check_nf_with(r);
        self.check_zf_with(r);
    }

    pub fn rla(&mut self, am: &AddressingMode) {
        self.rol(am);
        self.and(am);
    }

    pub fn rra(&mut self, am: &AddressingMode) {
        self.ror(am);
        self.adc(am);
    }

    pub fn sax(&mut self, am: &AddressingMode) {
        let r = self.x & self.a;
        self.write_op(am, r);
    }

    pub fn slo(&mut self, am: &AddressingMode) {
        self.asl(am);
        self.ora(am);
    }

    pub fn sre(&mut self, am: &AddressingMode) {
        self.lsr(am);
        self.eor(am);
    }

    // Private methods

    fn compare(&mut self, am: &AddressingMode, reg: u8) {
        let m = self.read_op(am);
        let c = reg.wrapping_sub(m);
        self.flags.set(Flag::Carry, reg >= m);
        self.check_nf_with(c);
        self.check_zf_with(c);
    }

    fn branch_on_flag(&mut self, am: &AddressingMode, f: Flag, value: bool) {
        if self.flags.has_set(f) == value {
            self.pc = self.resolve_mem_address(am);
        }
    }

    fn check_zf_with(&mut self, val: u8) {
        self.flags.set(Flag::Zero, val == 0);
    }

    fn check_nf_with(&mut self, val: u8) {
        self.flags.set(Flag::Negative, (val & 0x80) == 0x80);
    }

    fn read_op(&self, am: &AddressingMode) -> u8 {
        use self::AddressingMode::*;

        if am.has_to_access_memory() {
            let address = self.resolve_mem_address(am);
            self.mem.read(address)
        } else {
            match *am {
                AccA  => self.a,
                AccX  => self.x,
                AccY  => self.y,
                AccSP => self.sp,
                Immediate { o } => o,
                _ => panic!("Exhausted addressing modes in #read_op")
            }
        }
    }

    fn push_st_16(&mut self, value: u16) {
        let hi = (value >> 8) as u8;
        self.push_st_8(hi);
        let lo = value as u8;
        self.push_st_8(lo);
    }

    fn pop_st_16(&mut self) -> u16 {
        let lo = self.pop_st_8();
        let hi = self.pop_st_8();
        ((hi as u16) << 8) | lo as u16
    }

    fn push_st_8(&mut self, value: u8) -> u8 {
        self.mem.push_to_stack(self.sp, value);
        self.sp -= 1;
        value
    }

    fn pop_st_8(&mut self) -> u8 {
        self.sp += 1;
        self.mem.peek_from_stack(self.sp)
    }

    fn write_op(&mut self, am: &AddressingMode, value: u8) -> u8 {
        use self::AddressingMode::*;

        if am.has_to_access_memory() {
            let address = self.resolve_mem_address(am);
            self.mem.write(address, value)
        } else {
            match *am {
                AccA  => {
                    self.a = value;
                    value
                },
                AccX  => {
                    self.x = value;
                    value
                },
                AccY  => {
                    self.y = value;
                    value
                },
                AccSP => {
                    self.sp = value;
                    value
                },
                _ => panic!("Exhausted addressing modes in #write_op")
            }
        }
    }

    fn resolve_mem_address(&self, am: &AddressingMode) -> u16 {
        use self::AddressingMode::*;

        let address8to16 = |l: u8, h: u8| ((h as u16) << 8) | l as u16;
        let hi_byte_adr_from = |lo_byte_adr: u16, l: u8, h: u8| {
            if self.jmp_bug && l == 0xff {
                address8to16(0, h)
            } else {
                lo_byte_adr + 1
            }
        };

        match *am {
            Abs { l, h } => {
                let address = address8to16(l, h);
                address
            },
            AbsX { l, h } => {
                let address = address8to16(l, h).wrapping_add(address8to16(self.x, 0));
                address
            },
            AbsY { l, h } => {
                let address = address8to16(l, h).wrapping_add(address8to16(self.y, 0));
                address
            },
            ZeroPage { o } => {
                let address = address8to16(o, 0);
                address
            },
            ZeroPageX { o } => {
                let address = address8to16(o.wrapping_add(self.x), 0);
                address
            },
            ZeroPageY { o } => {
                let address = address8to16(o.wrapping_add(self.y), 0);
                address
            },
            Relative { o } => {
                let address = self.pc.wrapping_add(o as u16);
                address
            },
            Indirect { l, h } => {
                let ind_address = address8to16(l, h);
                let lo = self.mem.read(ind_address);
                let hi = self.mem.read(hi_byte_adr_from(ind_address, l, h));
                let address = address8to16(lo, hi);
                address
            },
            IndirectIndexed { a } => {
                let ind_address = address8to16(a, 0);
                let lo = self.mem.read(ind_address);
                let hi = self.mem.read(hi_byte_adr_from(ind_address, a, 0));
                let address = address8to16(lo, hi).wrapping_add(address8to16(self.y, 0));
                address
            },
            IndexedIndirect { a } => {
                let lo_byte_adr = a.wrapping_add(self.x);
                let ind_address = address8to16(lo_byte_adr, 0);
                let lo = self.mem.read(ind_address);
                let hi = self.mem.read(hi_byte_adr_from(ind_address, lo_byte_adr, 0));
                let address = address8to16(lo, hi);
                address
            },
            _ => panic!("{} is not a memory type address mode")
        }
    }

    fn fetch_address_from_vector(&self, vector: u16) -> u16 {
        let lo = self.mem.read(vector);
        let hi = self.mem.read(vector + 1);
        ((hi as u16) << 8) | lo as u16
    }


    #[allow(dead_code)]
    fn crash_and_dump(&self) {
        println!("{:?}", self.mem);
        panic!("Forced crash!");
    }
}

impl fmt::Debug for CPU {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<a: {:02x}, x: {:02x}, y: {:02x}, flags: {:?}, sp: {:02x}, pc: {:04x}>",
            self.a, self.x, self.y, self.flags, self.sp, self.pc)
    }
}
