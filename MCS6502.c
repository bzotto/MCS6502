//
//  MCS6502.c
//
//  Created by Ben Zotto on 3/19/19.
//  Copyright © 2019 Ben Zotto. All rights reserved.
//

#include <string.h>
#include "MCS6502.h"

//#define PRINT_DEBUG_OUTPUT

//
// 6502 Instruction Set
//

typedef enum _MCS6502AddressingMode {
    MCS6502AddressingImplied,          // no operand
    MCS6502AddressingAccumulator,      // operand is accumulator (implied)
    MCS6502AddressingImmediate,        // operand is single byte actual value
    MCS6502AddressingZeroPage,         // operand is a zero page address (high byte of addr is 0x00)
    MCS6502AddressingZeroPageX,        // operand is a zpg address, effective address is addr incremented by X (no carry)
    MCS6502AddressingZeroPageY,        // operand is a zpg address, effective address is addr incremented by Y (no carry)
    MCS6502AddressingAbsolute,         // operand is a complete (16-bit) address
    MCS6502AddressingAbsoluteX,        // operand is address, effective addr is addr incremented by X (with carry)
    MCS6502AddressingAbsoluteY,        // operand is address, effective addr is addr incremented by Y (with carry)
    MCS6502AddressingIndirect,         // operand is addrees, effective addr is the *contents* of addr. (ie, operand is a pointer)
    MCS6502AddressingXIndirect,        // operand is zpg address, effective addr is the 16bit word in (addr+X, addr+X+1) (no carry)
    MCS6502AddressingIndirectY,        // operand is zpg address, effective addr is the 16bit word in (addr, addr+1) + Y (w carry)
    MCS6502AddressingRelative          // operand (branch target) is PC + signed offset
} MCS6502AddressingMode;

typedef struct _MCS6502Instruction {
    uint8 opcode;
    char mnemonic[4];
    MCS6502AddressingMode mode;
    int timing;
    bool timingAddOne;
} MCS6502Instruction;

MCS6502Instruction MCS6502Instructions[] = {
    { 0x69, "ADC", MCS6502AddressingImmediate, 2, false },
    { 0x65, "ADC", MCS6502AddressingZeroPage, 3, false },
    { 0x75, "ADC", MCS6502AddressingZeroPageX, 4, false },
    { 0x6D, "ADC", MCS6502AddressingAbsolute, 4, false },
    { 0x7D, "ADC", MCS6502AddressingAbsoluteX, 4, true },
    { 0x79, "ADC", MCS6502AddressingAbsoluteY, 4, true },
    { 0x61, "ADC", MCS6502AddressingXIndirect, 6, false },
    { 0x71, "ADC", MCS6502AddressingIndirectY, 5, true },
    { 0x29, "AND", MCS6502AddressingImmediate, 2, false },
    { 0x25, "AND", MCS6502AddressingZeroPage, 3, false },
    { 0x35, "AND", MCS6502AddressingZeroPageX, 4, false },
    { 0x2D, "AND", MCS6502AddressingAbsolute, 4, false },
    { 0x3D, "AND", MCS6502AddressingAbsoluteX, 4, true },
    { 0x39, "AND", MCS6502AddressingAbsoluteY, 4, true },
    { 0x21, "AND", MCS6502AddressingXIndirect, 6, false },
    { 0x31, "AND", MCS6502AddressingIndirectY, 5, true },
    { 0x0A, "ASL", MCS6502AddressingAccumulator, 2, false },
    { 0x06, "ASL", MCS6502AddressingZeroPage, 5, false },
    { 0x16, "ASL", MCS6502AddressingZeroPageX, 6, false },
    { 0x0E, "ASL", MCS6502AddressingAbsolute, 6, false },
    { 0x1E, "ASL", MCS6502AddressingAbsoluteX, 7, false },
    { 0x90, "BCC", MCS6502AddressingRelative, 0, true },
    { 0xB0, "BCS", MCS6502AddressingRelative, 0, true },
    { 0xF0, "BEQ", MCS6502AddressingRelative, 0, true },
    { 0x24, "BIT", MCS6502AddressingZeroPage, 3, false },
    { 0x2C, "BIT", MCS6502AddressingAbsolute, 4, false },
    { 0x30, "BMI", MCS6502AddressingRelative, 0, true },
    { 0xD0, "BNE", MCS6502AddressingRelative, 0, true },
    { 0x10, "BPL", MCS6502AddressingRelative, 0, true },
    { 0x00, "BRK", MCS6502AddressingImplied, 7, false },
    { 0x50, "BVC", MCS6502AddressingRelative, 0, true },
    { 0x70, "BVS", MCS6502AddressingRelative, 0, true },
    { 0x18, "CLC", MCS6502AddressingImplied, 2, false },
    { 0xD8, "CLD", MCS6502AddressingImplied, 2, false },
    { 0x58, "CLI", MCS6502AddressingImplied, 2, false },
    { 0xB8, "CLV", MCS6502AddressingImplied, 2, false },
    { 0xC9, "CMP", MCS6502AddressingImmediate, 2, false },
    { 0xC5, "CMP", MCS6502AddressingZeroPage, 3, false },
    { 0xD5, "CMP", MCS6502AddressingZeroPageX, 4, false },
    { 0xCD, "CMP", MCS6502AddressingAbsolute, 4, false },
    { 0xDD, "CMP", MCS6502AddressingAbsoluteX, 4, true },
    { 0xD9, "CMP", MCS6502AddressingAbsoluteY, 4, true },
    { 0xC1, "CMP", MCS6502AddressingXIndirect, 6, false },
    { 0xD1, "CMP", MCS6502AddressingIndirectY, 5, true },
    { 0xE0, "CPX", MCS6502AddressingImmediate, 2, false },
    { 0xE4, "CPX", MCS6502AddressingZeroPage, 3, false },
    { 0xEC, "CPX", MCS6502AddressingAbsolute, 4, false },
    { 0xC0, "CPY", MCS6502AddressingImmediate, 3, false },
    { 0xC4, "CPY", MCS6502AddressingZeroPage, 3, false },
    { 0xCC, "CPY", MCS6502AddressingAbsolute, 4, false },
    { 0xC6, "DEC", MCS6502AddressingZeroPage, 5, false },
    { 0xD6, "DEC", MCS6502AddressingZeroPageX, 6, false },
    { 0xCE, "DEC", MCS6502AddressingAbsolute, 6, false },
    { 0xDE, "DEC", MCS6502AddressingAbsoluteX, 7, false },
    { 0xCA, "DEX", MCS6502AddressingImplied, 2, false },
    { 0x88, "DEY", MCS6502AddressingImplied, 2, false },
    { 0x49, "EOR", MCS6502AddressingImmediate, 2, false },
    { 0x45, "EOR", MCS6502AddressingZeroPage, 3, false },
    { 0x55, "EOR", MCS6502AddressingZeroPageX, 4, false },
    { 0x4D, "EOR", MCS6502AddressingAbsolute, 4, false },
    { 0x5D, "EOR", MCS6502AddressingAbsoluteX, 4, true },
    { 0x59, "EOR", MCS6502AddressingAbsoluteY, 4, true },
    { 0x41, "EOR", MCS6502AddressingXIndirect, 6, false },
    { 0x51, "EOR", MCS6502AddressingIndirectY, 5, true },
    { 0xE6, "INC", MCS6502AddressingZeroPage, 5, false },
    { 0xF6, "INC", MCS6502AddressingZeroPageX, 6, false },
    { 0xEE, "INC", MCS6502AddressingAbsolute, 6, false },
    { 0xFE, "INC", MCS6502AddressingAbsoluteX, 7, false },
    { 0xE8, "INX", MCS6502AddressingImplied, 2, false },
    { 0xC8, "INY", MCS6502AddressingImplied, 2, false },
    { 0x4C, "JMP", MCS6502AddressingAbsolute, 3, false },
    { 0x6C, "JMP", MCS6502AddressingIndirect, 5, false },
    { 0x20, "JSR", MCS6502AddressingAbsolute, 6, false },
    { 0xA9, "LDA", MCS6502AddressingImmediate, 2, false },
    { 0xA5, "LDA", MCS6502AddressingZeroPage, 3, false },
    { 0xB5, "LDA", MCS6502AddressingZeroPageX, 4, false },
    { 0xAD, "LDA", MCS6502AddressingAbsolute, 4, false },
    { 0xBD, "LDA", MCS6502AddressingAbsoluteX, 4, true },
    { 0xB9, "LDA", MCS6502AddressingAbsoluteY, 4, true },
    { 0xA1, "LDA", MCS6502AddressingXIndirect, 6, false },
    { 0xB1, "LDA", MCS6502AddressingIndirectY, 5, true },
    { 0xA2, "LDX", MCS6502AddressingImmediate, 2, false },
    { 0xA6, "LDX", MCS6502AddressingZeroPage, 3, false },
    { 0xB6, "LDX", MCS6502AddressingZeroPageY, 4, false },
    { 0xAE, "LDX", MCS6502AddressingAbsolute, 4, false },
    { 0xBE, "LDX", MCS6502AddressingAbsoluteY, 4, true },
    { 0xA0, "LDY", MCS6502AddressingImmediate, 2, false },
    { 0xA4, "LDY", MCS6502AddressingZeroPage, 3, false },
    { 0xB4, "LDY", MCS6502AddressingZeroPageX, 4, false },
    { 0xAC, "LDY", MCS6502AddressingAbsolute, 4, false },
    { 0xBC, "LDY", MCS6502AddressingAbsoluteX, 4, true },
    { 0x4A, "LSR", MCS6502AddressingAccumulator, 2, false },
    { 0x46, "LSR", MCS6502AddressingZeroPage, 5, false },
    { 0x56, "LSR", MCS6502AddressingZeroPageX, 6, false },
    { 0x4E, "LSR", MCS6502AddressingAbsolute, 6, false },
    { 0x5E, "LSR", MCS6502AddressingAbsoluteX, 7, false },
    { 0xEA, "NOP", MCS6502AddressingImplied, 2, false },
    { 0x09, "ORA", MCS6502AddressingImmediate, 2, false },
    { 0x05, "ORA", MCS6502AddressingZeroPage, 3, false },
    { 0x15, "ORA", MCS6502AddressingZeroPageX, 4, false },
    { 0x0D, "ORA", MCS6502AddressingAbsolute, 4, false },
    { 0x1D, "ORA", MCS6502AddressingAbsoluteX, 4, true },
    { 0x19, "ORA", MCS6502AddressingAbsoluteY, 4, true },
    { 0x01, "ORA", MCS6502AddressingXIndirect, 6, false },
    { 0x11, "ORA", MCS6502AddressingIndirectY, 5, true },
    { 0x48, "PHA", MCS6502AddressingImplied, 3, false },
    { 0x08, "PHP", MCS6502AddressingImplied, 3, false },
    { 0x68, "PLA", MCS6502AddressingImplied, 4, false },
    { 0x28, "PLP", MCS6502AddressingImplied, 4, false },
    { 0x2A, "ROL", MCS6502AddressingAccumulator, 2, false },
    { 0x26, "ROL", MCS6502AddressingZeroPage, 5, false },
    { 0x36, "ROL", MCS6502AddressingZeroPageX, 6, false },
    { 0x2E, "ROL", MCS6502AddressingAbsolute, 6, false },
    { 0x3E, "ROL", MCS6502AddressingAbsoluteX, 7, false },
    { 0x6A, "ROR", MCS6502AddressingAccumulator, 2, false },
    { 0x66, "ROR", MCS6502AddressingZeroPage, 5, false },
    { 0x76, "ROR", MCS6502AddressingZeroPageX, 6, false },
    { 0x6E, "ROR", MCS6502AddressingAbsolute, 6, false },
    { 0x7E, "ROR", MCS6502AddressingAbsoluteX, 7, false },
    { 0x40, "RTI", MCS6502AddressingImplied, 6, false },
    { 0x60, "RTS", MCS6502AddressingImplied, 6, false },
    { 0xE9, "SBC", MCS6502AddressingImmediate, 2, false },
    { 0xE5, "SBC", MCS6502AddressingZeroPage, 3, false },
    { 0xF5, "SBC", MCS6502AddressingZeroPageX, 4, false },
    { 0xED, "SBC", MCS6502AddressingAbsolute, 4, false },
    { 0xFD, "SBC", MCS6502AddressingAbsoluteX, 4, true },
    { 0xF9, "SBC", MCS6502AddressingAbsoluteY, 4, true },
    { 0xE1, "SBC", MCS6502AddressingXIndirect, 6, false },
    { 0xF1, "SBC", MCS6502AddressingIndirectY, 5, true },
    { 0x38, "SEC", MCS6502AddressingImplied, 2, false },
    { 0xF8, "SED", MCS6502AddressingImplied, 2, false },
    { 0x78, "SEI", MCS6502AddressingImplied, 2, false },
    { 0x85, "STA", MCS6502AddressingZeroPage, 3, false },
    { 0x95, "STA", MCS6502AddressingZeroPageX, 4, false },
    { 0x8D, "STA", MCS6502AddressingAbsolute, 4, false },
    { 0x9D, "STA", MCS6502AddressingAbsoluteX, 5, false },
    { 0x99, "STA", MCS6502AddressingAbsoluteY, 5, false },
    { 0x81, "STA", MCS6502AddressingXIndirect, 6, false },
    { 0x91, "STA", MCS6502AddressingIndirectY, 6, false },
    { 0x86, "STX", MCS6502AddressingZeroPage, 3, false },
    { 0x96, "STX", MCS6502AddressingZeroPageY, 4, false },
    { 0x8E, "STX", MCS6502AddressingAbsolute, 4, false },
    { 0x84, "STY", MCS6502AddressingZeroPage, 3, false },
    { 0x94, "STY", MCS6502AddressingZeroPageX, 4, false },
    { 0x8C, "STY", MCS6502AddressingAbsolute, 4, false },
    { 0xAA, "TAX", MCS6502AddressingImplied, 2, false },
    { 0xA8, "TAY", MCS6502AddressingImplied, 2, false },
    { 0xBA, "TSX", MCS6502AddressingImplied, 2, false },
    { 0x8A, "TXA", MCS6502AddressingImplied, 2, false },
    { 0x9A, "TXS", MCS6502AddressingImplied, 2, false },
    { 0x98, "TYA", MCS6502AddressingImplied, 2, false }
};

// Local (private) helper objects and functions.
static MCS6502Instruction *MCS6502OpcodeTable[256];
// Interrupts
static void HandleIRQ(MCS6502CPU * cpu, MCS6502DataBus * dataBus);
static void HandleNMI(MCS6502CPU * cpu, MCS6502DataBus * dataBus);
// Routines used in performing instructions
static inline void PushByte(uint8 byte, MCS6502CPU * cpu, MCS6502DataBus * dataBus);
static inline void PushFlags(bool b, MCS6502CPU * cpu, MCS6502DataBus * dataBus);
static inline uint8 PullByte(MCS6502CPU * cpu, MCS6502DataBus * dataBus);
static uint16 EffectiveOperandAddressForInstruction(MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus);
static uint8 OperandValueForCurrentInstruction(MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus);
static void WriteResultForCurrentInstruction(uint8 result, MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus);
static int LengthForInstruction(MCS6502Instruction * instruction);
static inline uint16 ReadWordAtAddress(MCS6502DataBus * dataBus, uint16 addr);
static inline void ExecuteConditionalBranch(int condition, MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus);
static inline uint8 BCDFromInteger(int integer);
static inline int IntegerFromBCD(uint8 bcd);
// Flag handling helpers
static inline void SetCarry(MCS6502CPU * cpu) { cpu->p |= MCS6502_STATUS_C; }
static inline void ClearCarry(MCS6502CPU * cpu) { cpu->p &= (~MCS6502_STATUS_C); }
static inline bool IsCarrySet(MCS6502CPU * cpu) { return (cpu->p & MCS6502_STATUS_C) > 0; }
static inline void SetOrClearCarry(MCS6502CPU * cpu, bool c);
static inline void SetZero(MCS6502CPU * cpu) { cpu->p |= MCS6502_STATUS_Z; }
static inline void ClearZero(MCS6502CPU * cpu) { cpu->p &= (~MCS6502_STATUS_Z); }
static inline bool IsZeroSet(MCS6502CPU * cpu) { return (cpu->p & MCS6502_STATUS_Z) > 0; }
static inline void UpdateZero(MCS6502CPU * cpu, uint8 val);
static inline void SetInterruptDisable(MCS6502CPU * cpu) { cpu->p |= MCS6502_STATUS_I; }
static inline void ClearInterruptDisable(MCS6502CPU * cpu) { cpu->p &= (~MCS6502_STATUS_I); }
static inline bool IsInterruptDisableSet(MCS6502CPU * cpu) { return (cpu->p & MCS6502_STATUS_I) > 0; }
static inline void SetDecimal(MCS6502CPU * cpu) { cpu->p |= MCS6502_STATUS_D; }
static inline void ClearDecimal(MCS6502CPU * cpu) { cpu->p &= (~MCS6502_STATUS_D); }
static inline bool IsDecimalSet(MCS6502CPU * cpu) { return (cpu->p & MCS6502_STATUS_D) > 0; }
static inline void ClearNegative(MCS6502CPU * cpu) { cpu->p &= (~MCS6502_STATUS_N); }
static inline void UpdateNegative(MCS6502CPU * cpu, uint8 val);
static inline bool IsNegativeSet(MCS6502CPU * cpu) { return (cpu->p & MCS6502_STATUS_N) > 0; }
static inline void ClearOverflow(MCS6502CPU * cpu) { cpu->p &= (~MCS6502_STATUS_V); }
static inline bool IsOverflowSet(MCS6502CPU * cpu) { return (cpu->p & MCS6502_STATUS_V) > 0; }
static inline void SetOrClearOverflow(MCS6502CPU * cpu, bool v);
static inline void UpdateZeroNegative(MCS6502CPU * cpu, uint8 val);
// DEBUG
char * DisassembleCurrentInstruction(MCS6502Instruction * instruction, MCS6502CPU * cpu, MCS6502DataBus * dataBus);

// Macros to simplify the readability of the call site for data bus access.
// Assumes a expression of type MCS6502DataBus* named "dataBus" is in scope.
// This is sort of janky but I am still figuring out how to do it more cleanly
// so it's readable 
#define DATABUS_READ_BYTE(addr)     (dataBus->readByte((addr), dataBus->context))
#define DATABUS_WRITE_BYTE(addr, b) (dataBus->writeByte((addr), (b), dataBus->context))

//
// Public functions
//

void
MCS6502Init(
    MCS6502CPU * cpu
)
{
    // Blank out the CPU state.
    memset(cpu, sizeof(MCS6502CPU), 0);
    
    // Set up static sorted opcode jump table. Zero out the table first so that
    // all unoccupied slots (invalid opcodes) will be NULL. Calling init more
    // than once doesn't hurt anything so no need to prevent double setup.
    memset(&MCS6502OpcodeTable[0], sizeof(MCS6502OpcodeTable), 0);
    int instructionCount = sizeof(MCS6502Instructions)/sizeof(MCS6502Instructions[0]);
    for (int i = 0; i < instructionCount; i++) {
        MCS6502Instruction * instruction = &MCS6502Instructions[i];
        MCS6502OpcodeTable[instruction->opcode] = instruction;
    }
}

void MCS6502Reset(MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    cpu->a = 0;
    cpu->x = 0;
    cpu->y = 0;
    cpu->p = MCS6502_STATUS_I;  // 6502 starts with interrupts disabled.
    
    // Stack pointer starts at top of page 1, but lands at 0xFD (rather than 0xFF)
    // after the hardware startup is complete.
    cpu->sp = 0xFD;
    
    // Jump to the address given at the reset vector location.
    cpu->pc = ReadWordAtAddress(dataBus, MCS6502_RESET);
}

void MCS6502IRQ(MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    if (IsInterruptDisableSet(cpu)) {
        return;
    }
    
    if (cpu->timing > 0) {
        cpu->irqPending = true;
        return;
    }
    
    HandleIRQ(cpu, dataBus);
}

void MCS6502NMI(MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    if (cpu->timing > 0) {
        cpu->nmiPending = true;
        return;
    }
    
    HandleNMI(cpu, dataBus);
}

MCS6502ExecResult
MCS6502Tick(
    MCS6502CPU * cpu,
    MCS6502DataBus * dataBus
)
{
    // If there are ticks pending to run down, decrement and leave.
    if (cpu->timing > 0) {
        cpu->timing--;
        if (cpu->timing == 0) {
            return MCS6502ExecResultCompleted;
        }
        return MCS6502ExecResultPending;
    }
    
    // If an interrupt is scheduled, do that instead of proceeding with
    // standard fetch. NMI takes precedence.
    if (cpu->nmiPending) {
        HandleNMI(cpu, dataBus);
        cpu->timing--;
        return MCS6502ExecResultPending;
    }
    
    if (cpu->irqPending) {
        HandleIRQ(cpu, dataBus);
        cpu->timing--;
        return MCS6502ExecResultPending;
    }
    
    // Fetch opcode
    uint8 opcode = DATABUS_READ_BYTE(cpu->pc);
    MCS6502Instruction * instruction = MCS6502OpcodeTable[opcode];
    if (!instruction) {
        return MCS6502ExecResultInvalidOperation;
    }
    
#ifdef PRINT_DEBUG_OUTPUT
    char * dis = DisassembleCurrentInstruction(instruction, cpu, dataBus);
    printf("%04X: %s\n", cpu->pc, dis);
#endif
    
    // All instructions will update the PC based on the length of the instruction,
    // except instructions that modify the PC directly. Flag those situations so
    // we can suppress the default behavior later.
    int doNotUpdatePC = 0;
    uint16 originalPC = cpu->pc;
    
    // Decode and execute op
    switch (opcode) {
        // ADC
        case 0x69:
        case 0x65:
        case 0x75:
        case 0x6D:
        case 0x7D:
        case 0x79:
        case 0x61:
        case 0x71:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
         
            if (!IsDecimalSet(cpu)) {
                unsigned int sum = cpu->a + operand + (IsCarrySet(cpu) ? 1 : 0);
                // Overflow flag is set if the twos complement (signed) result is > +127 or < -128.
                // The simplest way for us to do this is just do the addition as signed values and check.
                int vres = (signed char)cpu->a + (signed char)operand + (IsCarrySet(cpu) ? 1 : 0);
                cpu->a = sum & 0xFF;
                SetOrClearCarry(cpu, (sum > 0xFF));
                UpdateZeroNegative(cpu, cpu->a);
                SetOrClearOverflow(cpu, (vres > 127 || vres < -128));
            } else {
                int operandInteger = IntegerFromBCD(operand);
                int accInteger = IntegerFromBCD(cpu->a);
                int sum = operandInteger + accInteger + (IsCarrySet(cpu) ? 1 : 0);
                if (sum > 99) {
                    SetCarry(cpu);
                    sum -= 100;
                } else {
                    ClearCarry(cpu);
                }
                cpu->a = BCDFromInteger(sum);
                UpdateZeroNegative(cpu, cpu->a);
                // Don't do anything to overflow flag. It's not documented behavior.
            }
            break;
        }
            
        // AND
        case 0x29:
        case 0x25:
        case 0x35:
        case 0x2D:
        case 0x3D:
        case 0x39:
        case 0x21:
        case 0x31:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            cpu->a = cpu->a & operand;
            UpdateZeroNegative(cpu, cpu->a);
            break;
        }
            
        // ASL
        case 0x0A:
        case 0x06:
        case 0x16:
        case 0x0E:
        case 0x1E:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            SetOrClearCarry(cpu, (operand & 0x80) != 0);
            operand <<= 1;
            UpdateZeroNegative(cpu, operand);
            WriteResultForCurrentInstruction(operand, cpu, instruction, dataBus);
            break;
        }
            
        // BCC
        case 0x90:
        {
            ExecuteConditionalBranch(!IsCarrySet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BCS
        case 0xB0:
        {
            ExecuteConditionalBranch(IsCarrySet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BEQ
        case 0xF0:
        {
            ExecuteConditionalBranch(IsZeroSet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BIT
        case 0x24:
        case 0x2C:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            uint8 and = cpu->a & operand;
            UpdateNegative(cpu, operand);
            UpdateZero(cpu, and);
            SetOrClearOverflow(cpu, (operand & 0x40) > 0);
            break;
        }
            
        // BMI
        case 0x30:
        {
            ExecuteConditionalBranch(IsNegativeSet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BNE
        case 0xD0:
        {
            ExecuteConditionalBranch(!IsZeroSet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BPL
        case 0x10:
        {
            ExecuteConditionalBranch(!IsNegativeSet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BRK
        case 0x00:
        {
            uint16 nextPC = cpu->pc + 2;  // BRK can replace a 2-byte instruction; RTI returns to actual PC.
            PushByte((nextPC >> 8) & 0xFF, cpu, dataBus);
            PushByte(nextPC & 0xFF, cpu, dataBus);
            PushFlags(true, cpu, dataBus);
            SetInterruptDisable(cpu);
            cpu->pc = ReadWordAtAddress(dataBus, MCS6502_IRQ_BRK);
            doNotUpdatePC = 1;
            break;
        }
            
        // BVC
        case 0x50:
        {
            ExecuteConditionalBranch(!IsOverflowSet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // BVS
        case 0x70:
        {
            ExecuteConditionalBranch(IsOverflowSet(cpu), cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // CLC
        case 0x18:
        {
            ClearCarry(cpu);
            break;
        }
            
        // CLD
        case 0xD8:
        {
            ClearDecimal(cpu);
            break;
        }
            
        // CLI
        case 0x58:
        {
            ClearInterruptDisable(cpu);
            break;
        }
            
        // CLV
        case 0xB8:
        {
            ClearOverflow(cpu);
            break;
        }
            
        // CMP
        case 0xC9:
        case 0xC5:
        case 0xD5:
        case 0xCD:
        case 0xDD:
        case 0xD9:
        case 0xC1:
        case 0xD1:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            SetOrClearCarry(cpu, (cpu->a >= operand));
            UpdateZero(cpu, (cpu->a - operand));
            UpdateNegative(cpu, (cpu->a - operand));
            break;
        }
            
        // CPX
        case 0xE0:
        case 0xE4:
        case 0xEC:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            SetOrClearCarry(cpu, (cpu->x >= operand));
            UpdateZero(cpu, (cpu->x - operand));
            UpdateNegative(cpu, (cpu->x - operand));
            break;
        }
            
        // CPY
        case 0xC0:
        case 0xC4:
        case 0xCC:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            SetOrClearCarry(cpu, (cpu->y >= operand));
            UpdateZero(cpu, (cpu->y - operand));
            UpdateNegative(cpu, (cpu->y - operand));
            break;
        }
            
        // DEC
        case 0xC6:
        case 0xD6:
        case 0xCE:
        case 0xDE:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            operand--;
            WriteResultForCurrentInstruction(operand, cpu, instruction, dataBus);
            UpdateZeroNegative(cpu, operand);
            break;
        }
            
        // DEX
        case 0xCA:
        {
            cpu->x = cpu->x - 1;
            UpdateZeroNegative(cpu, cpu->x);
            break;
        }
            
        // DEY
        case 0x88:
        {
            cpu->y = cpu->y - 1;
            UpdateZeroNegative(cpu, cpu->y);
            break;
        }
            
        // EOR
        case 0x49:
        case 0x45:
        case 0x55:
        case 0x4D:
        case 0x5D:
        case 0x59:
        case 0x41:
        case 0x51:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            cpu->a = cpu->a ^ operand;
            UpdateZeroNegative(cpu, cpu->a);
            break;
        }
            
        // INC
        case 0xE6:
        case 0xF6:
        case 0xEE:
        case 0xFE:
        {
            //XXX: This simplified code doesn't "cache" the memory address between the
            // read and the result write, so we'll end up reading that twice. That's not
            // what the real CPU would do.
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            operand++;
            WriteResultForCurrentInstruction(operand, cpu, instruction, dataBus);
            UpdateZeroNegative(cpu, operand);
            break;
        }
            
        // INX
        case 0xE8:
        {
            cpu->x = cpu->x + 1;
            UpdateZeroNegative(cpu, cpu->x);
            break;
        }
            
        // INY
        case 0xC8:
        {
            cpu->y = cpu->y + 1;
            UpdateZeroNegative(cpu, cpu->y);
            break;
        }
            
        // JMP
        case 0x4C:
        case 0x6C:
        {
            cpu->pc = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
            doNotUpdatePC = 1;
            break;
        }
            
        // JSR
        case 0x20:
        {
            uint16 destAddr = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
            uint16 returnAddr = cpu->pc + 2;
            // Push address of last byte of the JSR instruction, high byte first.
            // RTS will always come back to address+1, hence the off by one push.
            PushByte(((returnAddr >> 8) & 0xFF), cpu, dataBus);
            PushByte((returnAddr & 0xFF), cpu, dataBus);
            cpu->pc = destAddr;
            doNotUpdatePC = 1;
            break;
        }
            
        // LDA
        case 0xA9:
        case 0xA5:
        case 0xB5:
        case 0xAD:
        case 0xBD:
        case 0xB9:
        case 0xA1:
        case 0xB1:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            cpu->a = operand;
            UpdateZeroNegative(cpu, operand);
            break;
        }
            
        // LDX
        case 0xA2:
        case 0xA6:
        case 0xB6:
        case 0xAE:
        case 0xBE:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            cpu->x = operand;
            UpdateZeroNegative(cpu, cpu->x);
            break;
        }
            
        // LDY
        case 0xA0:
        case 0xA4:
        case 0xB4:
        case 0xAC:
        case 0xBC:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            cpu->y = operand;
            UpdateZeroNegative(cpu, cpu->y);
            break;
        }
            
        // LSR
        case 0x4A:
        case 0x46:
        case 0x56:
        case 0x4E:
        case 0x5E:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            SetOrClearCarry(cpu, (operand & 0x01) != 0);
            operand >>= 1;
            operand &= 0x7F;
            UpdateZero(cpu, operand);
            ClearNegative(cpu);
            WriteResultForCurrentInstruction(operand, cpu, instruction, dataBus);
            break;
        }
            
        // NOP
        case 0xEA:
        {
            // NOTHING
            break;
        }
            
        // ORA
        case 0x09:
        case 0x05:
        case 0x15:
        case 0x0D:
        case 0x1D:
        case 0x19:
        case 0x01:
        case 0x11:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            cpu->a = cpu->a | operand;
            UpdateZeroNegative(cpu, cpu->a);
            break;
        }
            
        // PHA
        case 0x48:
        {
            PushByte(cpu->a, cpu, dataBus);
            break;
        }
            
        // PHP
        case 0x08:
        {
            PushFlags(true, cpu, dataBus);
            break;
        }
            
        // PLA
        case 0x68:
        {
            cpu->a = PullByte(cpu, dataBus);
            UpdateZeroNegative(cpu, cpu->a);
            break;
        }
            
        // PLP
        case 0x28:
        {
            uint8 p = PullByte(cpu, dataBus);
            p &= ~(0x20 | MCS6502_STATUS_B);
            cpu->p = p;
            break;
        }
            
        // ROL
        case 0x2A:
        case 0x26:
        case 0x36:
        case 0x2E:
        case 0x3E:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            int initialCarry = IsCarrySet(cpu);
            SetOrClearCarry(cpu, (operand & 0x80) > 0);
            operand <<= 1;
            operand &= 0xFE;
            operand |= (initialCarry ? 0x1 : 0);
            WriteResultForCurrentInstruction(operand, cpu, instruction, dataBus);
            UpdateZeroNegative(cpu, operand);
            break;
        }
            
        // ROR
        case 0x6A:
        case 0x66:
        case 0x76:
        case 0x6E:
        case 0x7E:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            int initialCarry = IsCarrySet(cpu);
            SetOrClearCarry(cpu, (operand & 0x01) > 0);
            operand >>= 1;
            operand &= 0x7F;
            operand |= (initialCarry ? 0x80 : 0);
            WriteResultForCurrentInstruction(operand, cpu, instruction, dataBus);
            UpdateZeroNegative(cpu, operand);
            break;
        }
            
        // RTI
        case 0x40:
        {
            uint8 p = PullByte(cpu, dataBus);
            p &= ~(0x20 | MCS6502_STATUS_B);
            cpu->p = p;
            uint8 lo = PullByte(cpu, dataBus);
            uint8 hi = PullByte(cpu, dataBus);
            cpu->pc = ((hi << 8) | lo);
            doNotUpdatePC = 1;
            break;
        }
            
        // RTS
        case 0x60:
        {
            uint8 lo = PullByte(cpu, dataBus);
            uint8 hi = PullByte(cpu, dataBus);
            cpu->pc = ((hi << 8) | lo) + 1;
            doNotUpdatePC = 1;
            break;
        }
            
        // SBC
        case 0xE9:
        case 0xE5:
        case 0xF5:
        case 0xED:
        case 0xFD:
        case 0xF9:
        case 0xE1:
        case 0xF1:
        {
            uint8 operand = OperandValueForCurrentInstruction(cpu, instruction, dataBus);
            if (!IsDecimalSet(cpu)) {
                // SBC is apparently exactly equivalent to ADC with the operand as ones-complement inverted, so:
                operand = ~operand;
                //
                unsigned int sum = cpu->a + operand + (IsCarrySet(cpu) ? 1 : 0);
                // Overflow flag is set if the twos complement (signed) result is > +127 or < -128.
                // The simplest way for us to do this is just do the addition as signed values and check.
                int vres = (signed char)cpu->a + (signed char)operand + (IsCarrySet(cpu) ? 1 : 0);
                cpu->a = sum & 0xFF;
                SetOrClearCarry(cpu, sum > 0xFF);
                UpdateZeroNegative(cpu, cpu->a);
                SetOrClearOverflow(cpu, (vres > 127 || vres < -128));
            } else {
                int operandInteger = IntegerFromBCD(operand);
                int accInteger = IntegerFromBCD(cpu->a);
                int difference = accInteger - operandInteger - (IsCarrySet(cpu) ? 0 : 1);
                if (difference < 0) {
                    ClearCarry(cpu);
                    difference += 100;
                } else {
                    SetCarry(cpu);
                }
                cpu->a = BCDFromInteger(difference);
                UpdateZeroNegative(cpu, cpu->a);
                // Don't do anything to overflow flag. It's not documented behavior.
            }
            break;
        }
            
        // SEC
        case 0x38:
        {
            SetCarry(cpu);
            break;
        }
            
        // SED
        case 0xF8:
        {
            SetDecimal(cpu);
            break;
        }
            
        // SEI
        case 0x78:
        {
            SetInterruptDisable(cpu);
            break;
        }
            
        // STA
        case 0x85:
        case 0x95:
        case 0x8D:
        case 0x9D:
        case 0x99:
        case 0x81:
        case 0x91:
        {
            uint16 addr = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
            DATABUS_WRITE_BYTE(addr, cpu->a);
            break;
        }
            
        // STX
        case 0x86:
        case 0x96:
        case 0x8E:
        {
            uint16 addr = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
            DATABUS_WRITE_BYTE(addr, cpu->x);
            break;
        }
            
        // STY
        case 0x84:
        case 0x94:
        case 0x8C:
        {
            uint16 addr = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
            DATABUS_WRITE_BYTE(addr, cpu->y);
            break;
        }
            
        // TAX
        case 0xAA:
        {
            cpu->x = cpu->a;
            UpdateZeroNegative(cpu, cpu->x);
            break;
        }
            
        // TAY
        case 0xA8:
        {
            cpu->y = cpu->a;
            UpdateZeroNegative(cpu, cpu->y);
            break;
        }
            
        // TSX
        case 0xBA:
        {
            cpu->x = cpu->sp;
            UpdateZeroNegative(cpu, cpu->x);
            break;
        }
            
        // TXA
        case 0x8A:
        {
            cpu->a = cpu->x;
            UpdateZeroNegative(cpu, cpu->a);
            break;
        }
            
        // TXS
        case 0x9A:
        {
            cpu->sp = cpu->x;
            break;
        }
            
        // TYA
        case 0x98:
        {
            cpu->a = cpu->y;
            UpdateZeroNegative(cpu, cpu->a);
            break;
        }
    }
    
    // Update PC and timing
    if (!doNotUpdatePC) {
        cpu->pc = cpu->pc + LengthForInstruction(instruction);
    }
    
#ifdef PRINT_DEBUG_OUTPUT
    // Dump the CPU state
    printf("  PC=%04X SP=0x01%02X A=%02X X=%02X Y=%02X N=%d V=%d D=%d I=%d Z=%d C=%d\n",
           cpu->pc, cpu->sp, cpu->a, cpu->x, cpu->y,
           IsNegativeSet(cpu) ? 1 : 0,
           IsOverflowSet(cpu) ? 1 : 0,
           IsDecimalSet(cpu) ? 1 : 0,
           (cpu->p & MCS6502_STATUS_I) > 0 ? 1 : 0,
           IsZeroSet(cpu) ? 1 : 0,
           IsCarrySet(cpu) ? 1 : 0);
    
    // Dump stack
    if (cpu->sp < 0xFF) {
        printf("  STACK: ");
        for (uint sp = 0xFF; sp > cpu->sp; sp--) {
            printf("%02X ", DATABUS_READ_BYTE(0x0100 + sp));
        }
        printf("\n");
    }
#endif
    
    cpu->timing = cpu->timing + instruction->timing;
    cpu->timing = cpu->timing - 1;
    
    if (originalPC == cpu->pc) {
        return MCS6502ExecResultHalting;
    }

    return MCS6502ExecResultPending;
}

//
// Internal helper functions
//

static inline void PushByte(uint8 byte, MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    uint16 esp = 0x0100 + cpu->sp;
    DATABUS_WRITE_BYTE(esp, byte);
    cpu->sp = cpu->sp - 1;
}

static inline void PushFlags(bool b, MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    uint8 p = cpu->p;
    p |= 0x20; // bit 5 always set in pushed status
    if (b) {
        p |= MCS6502_STATUS_B;
    } else {
        p &= (~MCS6502_STATUS_B);
    }
    PushByte(p, cpu, dataBus);
}

static inline uint8 PullByte(MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    cpu->sp = cpu->sp + 1;
    uint16 esp = 0x0100 + cpu->sp;
    return DATABUS_READ_BYTE(esp);
}

static inline uint16 ReadWordAtAddress(MCS6502DataBus * dataBus, uint16 addr)
{
    uint8 veclo = DATABUS_READ_BYTE(addr);
    uint8 vechi = DATABUS_READ_BYTE(addr + 1);
    return (vechi << 8) | veclo;
}

static uint16 EffectiveOperandAddressForInstruction(MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus)
{
    switch (instruction->mode) {
        case MCS6502AddressingZeroPage:
        {
            return DATABUS_READ_BYTE(cpu->pc + 1);
        }
        case MCS6502AddressingZeroPageX:
        {
            return (uint16)(uint8)(DATABUS_READ_BYTE(cpu->pc + 1) + cpu->x);
        }
        case MCS6502AddressingZeroPageY:
        {
            return (uint16)(uint8)(DATABUS_READ_BYTE(cpu->pc + 1) + cpu->y);
        }
        case MCS6502AddressingAbsolute:
        {
            return ReadWordAtAddress(dataBus, cpu->pc + 1);
        }
        case MCS6502AddressingAbsoluteX:
        {
            uint16 addr = ReadWordAtAddress(dataBus, cpu->pc + 1);
            return addr + cpu->x;
        }
        case MCS6502AddressingAbsoluteY:
        {
            uint16 addr = ReadWordAtAddress(dataBus, cpu->pc + 1);
            return addr + cpu->y;
        }
        case MCS6502AddressingIndirect:
        {
            uint8 loIndirect = DATABUS_READ_BYTE(cpu->pc + 1);
            uint8 hiIndirect = DATABUS_READ_BYTE(cpu->pc + 2);
            uint16 indirect = (hiIndirect << 8 | loIndirect);
            // This mode has no carry so force a wrap in the hi address if it crosses
            // a page. User code should never do this because it's bad to, but we
            // will behave like the hardware would just in case.
            uint8 lo = DATABUS_READ_BYTE(indirect);
            uint8 hi;
            if (loIndirect == 0xFF) {
                hi = DATABUS_READ_BYTE(hiIndirect << 8);
            } else {
                hi = DATABUS_READ_BYTE(indirect + 1);
            }
            return (hi << 8 | lo);
        }
        case MCS6502AddressingXIndirect:
        {
            uint8 addr = (uint8)(cpu->x + DATABUS_READ_BYTE(cpu->pc + 1));
            return ReadWordAtAddress(dataBus, addr);
        }
        case MCS6502AddressingIndirectY:
        {
            uint8 addr = DATABUS_READ_BYTE(cpu->pc + 1);
            return ReadWordAtAddress(dataBus, addr) + cpu->y;
        }
        case MCS6502AddressingRelative:
        {
            signed char offset = (signed char)DATABUS_READ_BYTE(cpu->pc + 1);
            uint16 addr = cpu->pc + 2 + offset; // all relative instructions are 2 bytes long, and we need to account for that here.
            return addr;
        }
        // This function has no meaning for the following modes and isn't called for them.
        case MCS6502AddressingImmediate:
        case MCS6502AddressingAccumulator:
        case MCS6502AddressingImplied:
        default:
            ;
    }
    return 0;
}

static uint8 OperandValueForCurrentInstruction(MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus)
{
    if (instruction->mode == MCS6502AddressingImmediate) {
        return DATABUS_READ_BYTE(cpu->pc + 1);
    } else if (instruction->mode == MCS6502AddressingAccumulator) {
        return cpu->a;
    } else {
        uint16 addr = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
        return DATABUS_READ_BYTE(addr);
    }
}

static void WriteResultForCurrentInstruction(uint8 result, MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus)
{
   if (instruction->mode == MCS6502AddressingAccumulator) {
        cpu->a = result;
   } else {
        uint16 addr = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
       DATABUS_WRITE_BYTE(addr, result);
   }
}

static int LengthForInstruction(MCS6502Instruction * instruction)
{
    switch (instruction->mode) {
        case MCS6502AddressingImplied:
            return 1;
        case MCS6502AddressingImmediate:
            return 2;
        case MCS6502AddressingAccumulator:
            return 1;
        case MCS6502AddressingZeroPage:
            return 2;
        case MCS6502AddressingZeroPageX:
            return 2;
        case MCS6502AddressingZeroPageY:
            return 2;
        case MCS6502AddressingAbsolute:
            return 3;
        case MCS6502AddressingAbsoluteX:
            return 3;
        case MCS6502AddressingAbsoluteY:
            return 3;
        case MCS6502AddressingIndirect:
            return 3;
        case MCS6502AddressingXIndirect:
            return 2;
        case MCS6502AddressingIndirectY:
            return 2;
        case MCS6502AddressingRelative:
            return 2;
    }
}

static inline void SetOrClearCarry(MCS6502CPU * cpu, bool c)
{
    if (c) {
        cpu->p |= MCS6502_STATUS_C;
    } else {
        cpu->p &= (~MCS6502_STATUS_C);
    }
}

static inline void UpdateZero(MCS6502CPU * cpu, uint8 val)
{
    if (val == 0) {
        SetZero(cpu);
    } else {
        ClearZero(cpu);
    }
}

static inline void UpdateNegative(MCS6502CPU * cpu, uint8 val)
{
    if ((val & 0x80) > 0) {
        cpu->p |= MCS6502_STATUS_N;
    } else {
        cpu->p &= (~MCS6502_STATUS_N);
    }
}

static inline void SetOrClearOverflow(MCS6502CPU * cpu, bool v)
{
    if (v) {
        cpu->p |= MCS6502_STATUS_V;
    } else {
        cpu->p &= (~MCS6502_STATUS_V);
    }
}

static inline void UpdateZeroNegative(MCS6502CPU * cpu, uint8 val)
{
    UpdateZero(cpu, val);
    UpdateNegative(cpu, val);
}

static inline void ExecuteConditionalBranch(int condition, MCS6502CPU * cpu, MCS6502Instruction * instruction, MCS6502DataBus * dataBus)
{
    if (condition) {
        cpu->pc = EffectiveOperandAddressForInstruction(cpu, instruction, dataBus);
        cpu->timing = cpu->timing + 3;
    } else {
        cpu->pc = cpu->pc + LengthForInstruction(instruction);
        cpu->timing = cpu->timing + 2;
    }
}

static inline uint8 BCDFromInteger(int integer)
{
    if (integer >= 0 && integer < 100) {
        int tens = integer / 10;
        int units = integer % 10;
        return ((uint8)tens << 4 | (uint8)units);
    }
    return 0x00;
}

// NB This routine does not validate that the input is valid BCD.
//  You'll get garbage output if you give it invalid input.
static inline int IntegerFromBCD(uint8 bcd)
{
    int val = ((bcd & 0xF0) >> 4) * 10;
    val += (bcd & 0x0F);
    return val;
}

static void HandleIRQ(MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    cpu->irqPending = false;
    uint16 returnPC = cpu->pc;
    PushByte((returnPC >> 8) & 0xFF, cpu, dataBus);
    PushByte(returnPC & 0xFF, cpu, dataBus);
    PushFlags(false, cpu, dataBus);
    SetInterruptDisable(cpu);
    cpu->pc = ReadWordAtAddress(dataBus, MCS6502_IRQ_BRK);
    cpu->timing += 7;
}

static void HandleNMI(MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    cpu->nmiPending = false;
    uint16 returnPC = cpu->pc;
    PushByte((returnPC >> 8) & 0xFF, cpu, dataBus);
    PushByte(returnPC & 0xFF, cpu, dataBus);
    PushFlags(false, cpu, dataBus);
    SetInterruptDisable(cpu);
    cpu->pc = ReadWordAtAddress(dataBus, MCS6502_IRQ_BRK);
    cpu->timing += 7;
}

//
// DEBUG
//

char * DisassembleCurrentInstruction(MCS6502Instruction * instruction, MCS6502CPU * cpu, MCS6502DataBus * dataBus)
{
    static char scratch[256];
    switch (instruction->mode) {
        case MCS6502AddressingImmediate:
        {
            uint8 val = DATABUS_READ_BYTE(cpu->pc + 1);
            sprintf(scratch, "#$%02X", val);
            break;
        }
        case MCS6502AddressingAccumulator:
        {
            sprintf(scratch, "A");
            break;
        }
        case MCS6502AddressingIndirect:
        {
            uint8 lo = DATABUS_READ_BYTE(cpu->pc + 1);
            uint8 hi = DATABUS_READ_BYTE(cpu->pc + 2);
            sprintf(scratch, "($%02X%02X)", hi, lo);
            break;
        }
        case MCS6502AddressingAbsolute:
        {
            uint8 lo = DATABUS_READ_BYTE(cpu->pc + 1);
            uint8 hi = DATABUS_READ_BYTE(cpu->pc + 2);
            sprintf(scratch, "$%02X%02X", hi, lo);
            break;
        }
        case MCS6502AddressingZeroPage:
        {
            uint8 addr = DATABUS_READ_BYTE(cpu->pc + 1);
            sprintf(scratch, "$%02X", addr);
            break;
        }
        case MCS6502AddressingAbsoluteX:
        {
            uint8 lo = DATABUS_READ_BYTE(cpu->pc + 1);
            uint8 hi = DATABUS_READ_BYTE(cpu->pc + 2);
            sprintf(scratch, "$%02X%02X, X", hi, lo);
            break;
        }
        case MCS6502AddressingAbsoluteY:
        {
            uint8 lo = DATABUS_READ_BYTE(cpu->pc + 1);
            uint8 hi = DATABUS_READ_BYTE(cpu->pc + 2);
            sprintf(scratch, "$%02X%02X, Y", hi, lo);
            break;
        }
        case MCS6502AddressingZeroPageX:
        {
            uint8 addr = DATABUS_READ_BYTE(cpu->pc + 1);
            sprintf(scratch, "$%02X, X", addr);
            break;
        }
        case MCS6502AddressingZeroPageY:
        {
            uint8 addr = DATABUS_READ_BYTE(cpu->pc + 1);
            sprintf(scratch, "$%02X, Y", addr);
            break;
        }
        case MCS6502AddressingXIndirect:
        {
            uint8 addr = DATABUS_READ_BYTE(cpu->pc + 1);
            sprintf(scratch, "($%02X, X)", addr);
            break;
        }
        case MCS6502AddressingIndirectY:
        {
            uint8 addr = DATABUS_READ_BYTE(cpu->pc + 1);
            sprintf(scratch, "($%02X), Y", addr);
            break;
        }
        case MCS6502AddressingRelative:
        {
            signed char offset = (signed char)DATABUS_READ_BYTE(cpu->pc + 1);
            uint16 addr = cpu->pc + 2 + offset;
            sprintf(scratch, "$%04X", addr); // this is a special case, it's not really the disassembly
            break;
        }
        case MCS6502AddressingImplied:
        default:
            scratch[0] = '\0';
            break;
    }
    static char line[256];
    sprintf(line, "%s %s", instruction->mnemonic, scratch);
    return line;
}
