//
//  MCS6502.c
//
//  Copyright (c) 2019 by Ben Zotto
//
//  Usage is subject to the MIT License, and copyright credit must be noted
//  in the documenation of binary redistributions. See accompanying LICENSE.TXT.
//

#include <string.h>
#include "MCS6502.h"

// If you uncomment the below and rebuild, you'll get a stream of
// disassembly, register and stack state printed to stdout:
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
// The above instructions are inserted into this indexed jump table the
// first time someone calls init, below.
static MCS6502Instruction *MCS6502OpcodeTable[256];

//
// Forward declarations of local private routines.
//

static inline uint8 MCS6502ReadByte(uint16 addr, MCS6502ExecutionContext * ctx);
static inline void MCS6502WriteByte(uint16 addr, uint8 byte, MCS6502ExecutionContext * ctx);
static inline void PushByte(uint8 byte, MCS6502ExecutionContext * context);
static inline void PushFlags(bool b, MCS6502ExecutionContext * context);
static inline uint8 PullByte(MCS6502ExecutionContext * context);
static inline uint16 ReadWordAtAddress(uint16 addr, MCS6502ExecutionContext * context);
static inline uint8 BCDFromInteger(int integer);
static inline int IntegerFromBCD(uint8 bcd);

static void HandleIRQ(MCS6502ExecutionContext * context);
static void HandleNMI(MCS6502ExecutionContext * context);

static uint16 EffectiveOperandAddressForInstruction(MCS6502Instruction * instruction, MCS6502ExecutionContext * context, bool * crossesPageBoundary);
static uint8 ReadOperandValueForCurrentInstruction(MCS6502Instruction * instruction, MCS6502ExecutionContext * context);
static void WriteResultForCurrentInstruction(uint8 result, MCS6502Instruction * instruction, MCS6502ExecutionContext * context);
static int LengthForInstruction(MCS6502Instruction * instruction);
static void ExecuteConditionalBranch(bool condition, MCS6502Instruction * instruction, MCS6502ExecutionContext * context);

//
// Status flag helpers.
//

#define CTXP_SET(f) (context->p |= (f))
#define CTXP_CLEAR(f) (context->p &= (~(f)))
#define CTXP_ISSET(f) ((context->p & (f)) != 0)
static inline void SetCarry(MCS6502ExecutionContext * context) { CTXP_SET(MCS6502_STATUS_C); }
static inline void ClearCarry(MCS6502ExecutionContext * context) { CTXP_CLEAR(MCS6502_STATUS_C); }
static inline bool IsCarrySet(MCS6502ExecutionContext * context) { return CTXP_ISSET(MCS6502_STATUS_C); }
static inline void SetOrClearCarry(bool c, MCS6502ExecutionContext * context) { if (c) { SetCarry(context); } else { ClearCarry(context); } }

static inline void SetZero(MCS6502ExecutionContext * context) { CTXP_SET(MCS6502_STATUS_Z); }
static inline void ClearZero(MCS6502ExecutionContext * context) { CTXP_CLEAR(MCS6502_STATUS_Z); }
static inline bool IsZeroSet(MCS6502ExecutionContext * context) { return CTXP_ISSET(MCS6502_STATUS_Z); }
static inline void UpdateZero(uint8 val, MCS6502ExecutionContext * context) { if (val == 0) { SetZero(context); } else { ClearZero(context); } }

static inline void SetInterruptDisable(MCS6502ExecutionContext * context) { CTXP_SET(MCS6502_STATUS_I);  }
static inline void ClearInterruptDisable(MCS6502ExecutionContext * context) { CTXP_CLEAR(MCS6502_STATUS_I); }
static inline bool IsInterruptDisableSet(MCS6502ExecutionContext * context) { return CTXP_ISSET(MCS6502_STATUS_I); }

static inline void SetDecimal(MCS6502ExecutionContext * context) { CTXP_SET(MCS6502_STATUS_D); }
static inline void ClearDecimal(MCS6502ExecutionContext * context) { CTXP_CLEAR(MCS6502_STATUS_D); }
static inline bool IsDecimalSet(MCS6502ExecutionContext * context) { return CTXP_ISSET(MCS6502_STATUS_D); }

static inline void SetNegative(MCS6502ExecutionContext * context) { CTXP_SET(MCS6502_STATUS_N); }
static inline void ClearNegative(MCS6502ExecutionContext * context) { CTXP_CLEAR(MCS6502_STATUS_N); }
static inline bool IsNegativeSet(MCS6502ExecutionContext * context) { return CTXP_ISSET(MCS6502_STATUS_N); }
static inline void UpdateNegative(uint8 val, MCS6502ExecutionContext * context) { if ((val & 0x80) != 0) { SetNegative(context); } else { ClearNegative(context); } }

static inline void SetOverflow(MCS6502ExecutionContext * context) { CTXP_SET(MCS6502_STATUS_V); }
static inline void ClearOverflow(MCS6502ExecutionContext * context) { CTXP_CLEAR(MCS6502_STATUS_V); }
static inline bool IsOverflowSet(MCS6502ExecutionContext * context) { return CTXP_ISSET(MCS6502_STATUS_V); }
static inline void SetOrClearOverflow(bool v, MCS6502ExecutionContext * context) { if (v) { SetOverflow(context); } else { ClearOverflow(context); } }

static inline void UpdateZeroNegative(uint8 val, MCS6502ExecutionContext * context) { UpdateZero(val, context); UpdateNegative(val, context); }
#undef CTXP_SET
#undef CTXP_CLEAR
#undef CTXP_ISSET

// Debug helper:
char * DisassembleCurrentInstruction(MCS6502Instruction * instruction, MCS6502ExecutionContext * context);

//
// Public functions
//

void
MCS6502Init(
    MCS6502ExecutionContext * context,
    MCS6502DataReadByteFunction readByteFn,
    MCS6502DataWriteByteFunction writeByteFn,
    void * readWriteContext
)
{
    // Blank out the context state.
    memset(context, 0, sizeof(MCS6502ExecutionContext));
    
    // Setup the data access functions.
    context->readByte = readByteFn;
    context->writeByte = writeByteFn;
    context->readWriteContext = readWriteContext;
    
    // Set up our static sorted opcode jump table. Zero out the table first so that
    // all unoccupied slots (invalid opcodes) will be NULL.
    static bool opcodesReady = false;
    if (!opcodesReady) {
        memset(&MCS6502OpcodeTable[0], 0, sizeof(MCS6502OpcodeTable));
        int instructionCount = sizeof(MCS6502Instructions)/sizeof(MCS6502Instructions[0]);
        for (int i = 0; i < instructionCount; i++) {
            MCS6502Instruction * instruction = &MCS6502Instructions[i];
            MCS6502OpcodeTable[instruction->opcode] = instruction;
        }
        opcodesReady = true;
    }
}

void
MCS6502Reset(
    MCS6502ExecutionContext * context
)
{
    context->a = 0;
    context->x = 0;
    context->y = 0;
    context->p = MCS6502_STATUS_I;  // 6502 starts with interrupts disabled.
    
    // Stack pointer starts at top of page 1, but lands at 0xFD (rather than 0xFF)
    // after the hardware startup is complete.
    context->sp = 0xFD;
    
    // Jump to the address given at the reset vector location.
    context->pc = ReadWordAtAddress(MCS6502_RESET, context);
}

void
MCS6502IRQ(
   MCS6502ExecutionContext * context
)
{
    if (IsInterruptDisableSet(context)) {
        return;
    }
    
    if (context->pendingTiming > 0) {
        context->irqPending = true;
        return;
    }
    
    HandleIRQ(context);
    context->pendingTiming = context->timingForLastOperation;
}

void
MCS6502NMI(
   MCS6502ExecutionContext * context
)
{
    if (context->pendingTiming > 0) {
        context->nmiPending = true;
        return;
    }
    
    HandleNMI(context);
    context->pendingTiming = context->timingForLastOperation;
}

MCS6502ExecResult
MCS6502Tick(
    MCS6502ExecutionContext * context
)
{
    // If there are ticks pending to run down, decrement and leave.
    if (context->pendingTiming > 0) {
        context->pendingTiming--;
        return MCS6502ExecResultRunning;
    }
    
    // If nothing is pending, execute the next instruction.
    MCS6502ExecResult result = MCS6502ExecNext(context);
    
    // Assign the operation's cycle count to the pending count, then
    // decrement it to represent one tick.
    context->pendingTiming = context->timingForLastOperation;
    context->pendingTiming--;
    
    return result;
}

MCS6502ExecResult
MCS6502ExecNext(
    MCS6502ExecutionContext * context
)
{
    // We expect to be called either by the tick function when there is nothing
    // pending, or by an external caller directly who intends to immediately
    // perform the next instruction. Zero out the tick counts in case
    // we are looking at the latter situation, because it doesn't matter.
    context->pendingTiming = 0;
    
    // Zero out the timing for this coming operation. We may increment it in multiple
    // places depending on the addressing.
    context->timingForLastOperation = 0;
    
    // If an interrupt is scheduled, do that instead of proceeding with
    // standard fetch. NMI takes precedence.
    if (context->nmiPending) {
        HandleNMI(context);
        return MCS6502ExecResultRunning;
    }
    
    if (context->irqPending) {
        HandleIRQ(context);
        return MCS6502ExecResultRunning;
    }
    
    // Fetch opcode
    uint8 opcode = MCS6502ReadByte(context->pc, context);
    MCS6502Instruction * instruction = MCS6502OpcodeTable[opcode];
    if (!instruction) {
        return MCS6502ExecResultInvalidOperation;
    }
    
#ifdef PRINT_DEBUG_OUTPUT
    char * dis = DisassembleCurrentInstruction(instruction, context);
    printf("%04X: %s\n", context->pc, dis);
#endif
    
    // All instructions will update the PC based on the length of the instruction,
    // except instructions that modify the PC directly. Flag those situations so
    // we can suppress the default behavior later.
    bool doNotUpdatePC = false;
    
    // Track the original PC value so that we can known whether we are infinite-looping
    // on the same address (ie, a halt). An interrupt of some kind will kick the CPU
    // out of that state, but it's useful to be able to flag it.
    uint16 originalPC = context->pc;
    
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
         
            if (!IsDecimalSet(context)) {
                unsigned int sum = context->a + operand + (IsCarrySet(context) ? 1 : 0);
                // Overflow flag is set if the twos complement (signed) result is > +127 or < -128.
                // The simplest way for us to do this is just do the addition as signed values and check.
                int vres = (signed char)context->a + (signed char)operand + (IsCarrySet(context) ? 1 : 0);
                context->a = sum & 0xFF;
                SetOrClearCarry((sum > 0xFF), context);
                UpdateZeroNegative(context->a, context);
                SetOrClearOverflow((vres > 127 || vres < -128), context);
            } else {
                int operandInteger = IntegerFromBCD(operand);
                int accInteger = IntegerFromBCD(context->a);
                int sum = operandInteger + accInteger + (IsCarrySet(context) ? 1 : 0);
                if (sum > 99) {
                    SetCarry(context);
                    sum -= 100;
                } else {
                    ClearCarry(context);
                }
                context->a = BCDFromInteger(sum);
                UpdateZeroNegative(context->a, context);
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            context->a = context->a & operand;
            UpdateZeroNegative(context->a, context);
            break;
        }
            
        // ASL
        case 0x0A:
        case 0x06:
        case 0x16:
        case 0x0E:
        case 0x1E:
        {
            uint8 val = ReadOperandValueForCurrentInstruction(instruction, context);
            SetOrClearCarry((val & 0x80) != 0, context);
            val <<= 1;
            UpdateZeroNegative(val, context);
            WriteResultForCurrentInstruction(val, instruction, context);
            break;
        }
            
        // BCC
        case 0x90:
        {
            ExecuteConditionalBranch(!IsCarrySet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BCS
        case 0xB0:
        {
            ExecuteConditionalBranch(IsCarrySet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BEQ
        case 0xF0:
        {
            ExecuteConditionalBranch(IsZeroSet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BIT
        case 0x24:
        case 0x2C:
        {
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            uint8 and = context->a & operand;
            UpdateNegative(operand, context);
            UpdateZero(and, context);
            SetOrClearOverflow((operand & 0x40) > 0, context);
            break;
        }
            
        // BMI
        case 0x30:
        {
            ExecuteConditionalBranch(IsNegativeSet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BNE
        case 0xD0:
        {
            ExecuteConditionalBranch(!IsZeroSet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BPL
        case 0x10:
        {
            ExecuteConditionalBranch(!IsNegativeSet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BRK
        case 0x00:
        {
            uint16 nextPC = context->pc + 2;  // BRK can replace a 2-byte instruction; RTI returns to actual PC.
            PushByte((nextPC >> 8) & 0xFF, context);
            PushByte(nextPC & 0xFF, context);
            PushFlags(true, context);
            SetInterruptDisable(context);
            context->pc = ReadWordAtAddress(MCS6502_IRQ_BRK, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BVC
        case 0x50:
        {
            ExecuteConditionalBranch(!IsOverflowSet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // BVS
        case 0x70:
        {
            ExecuteConditionalBranch(IsOverflowSet(context), instruction, context);
            doNotUpdatePC = true;
            break;
        }
            
        // CLC
        case 0x18:
        {
            ClearCarry(context);
            break;
        }
            
        // CLD
        case 0xD8:
        {
            ClearDecimal(context);
            break;
        }
            
        // CLI
        case 0x58:
        {
            ClearInterruptDisable(context);
            break;
        }
            
        // CLV
        case 0xB8:
        {
            ClearOverflow(context);
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            SetOrClearCarry((context->a >= operand), context);
            UpdateZero((context->a - operand), context);
            UpdateNegative((context->a - operand), context);
            break;
        }
            
        // CPX
        case 0xE0:
        case 0xE4:
        case 0xEC:
        {
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            SetOrClearCarry((context->x >= operand), context);
            UpdateZero((context->x - operand), context);
            UpdateNegative((context->x - operand), context);
            break;
        }
            
        // CPY
        case 0xC0:
        case 0xC4:
        case 0xCC:
        {
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            SetOrClearCarry((context->y >= operand), context);
            UpdateZero((context->y - operand), context);
            UpdateNegative((context->y - operand), context);
            break;
        }
            
        // DEC
        case 0xC6:
        case 0xD6:
        case 0xCE:
        case 0xDE:
        {
            uint8 val = ReadOperandValueForCurrentInstruction(instruction, context);
            val--;
            WriteResultForCurrentInstruction(val, instruction, context);
            UpdateZeroNegative(val, context);
            break;
        }
            
        // DEX
        case 0xCA:
        {
            context->x = context->x - 1;
            UpdateZeroNegative(context->x, context);
            break;
        }
            
        // DEY
        case 0x88:
        {
            context->y = context->y - 1;
            UpdateZeroNegative(context->y, context);
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            context->a = context->a ^ operand;
            UpdateZeroNegative(context->a, context);
            break;
        }
            
        // INC
        case 0xE6:
        case 0xF6:
        case 0xEE:
        case 0xFE:
        {
            uint8 value = ReadOperandValueForCurrentInstruction(instruction, context);
            value++;
            WriteResultForCurrentInstruction(value, instruction, context);
            UpdateZeroNegative(value, context);
            break;
        }
            
        // INX
        case 0xE8:
        {
            context->x = context->x + 1;
            UpdateZeroNegative(context->x, context);
            break;
        }
            
        // INY
        case 0xC8:
        {
            context->y = context->y + 1;
            UpdateZeroNegative(context->y, context);
            break;
        }
            
        // JMP
        case 0x4C:
        case 0x6C:
        {
            context->pc = EffectiveOperandAddressForInstruction(instruction, context, NULL);
            doNotUpdatePC = true;
            break;
        }
            
        // JSR
        case 0x20:
        {
            uint16 destAddr = EffectiveOperandAddressForInstruction(instruction, context, NULL);
            uint16 returnAddr = context->pc + 2;
            // Push address of last byte of the JSR instruction, high byte first.
            // RTS will always come back to address+1, hence the off by one push.
            PushByte(((returnAddr >> 8) & 0xFF), context);
            PushByte((returnAddr & 0xFF), context);
            context->pc = destAddr;
            doNotUpdatePC = true;
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            context->a = operand;
            UpdateZeroNegative(operand, context);
            break;
        }
            
        // LDX
        case 0xA2:
        case 0xA6:
        case 0xB6:
        case 0xAE:
        case 0xBE:
        {
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            context->x = operand;
            UpdateZeroNegative(context->x, context);
            break;
        }
            
        // LDY
        case 0xA0:
        case 0xA4:
        case 0xB4:
        case 0xAC:
        case 0xBC:
        {
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            context->y = operand;
            UpdateZeroNegative(context->y, context);
            break;
        }
            
        // LSR
        case 0x4A:
        case 0x46:
        case 0x56:
        case 0x4E:
        case 0x5E:
        {
            uint8 val = ReadOperandValueForCurrentInstruction(instruction, context);
            SetOrClearCarry((val & 0x01) != 0, context);
            val >>= 1;
            val &= 0x7F;
            UpdateZero(val, context);
            ClearNegative(context);
            WriteResultForCurrentInstruction(val, instruction, context);
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            context->a = context->a | operand;
            UpdateZeroNegative(context->a, context);
            break;
        }
            
        // PHA
        case 0x48:
        {
            PushByte(context->a, context);
            break;
        }
            
        // PHP
        case 0x08:
        {
            PushFlags(true, context);
            break;
        }
            
        // PLA
        case 0x68:
        {
            context->a = PullByte(context);
            UpdateZeroNegative(context->a, context);
            break;
        }
            
        // PLP
        case 0x28:
        {
            uint8 p = PullByte(context);
            p &= ~(0x20 | MCS6502_STATUS_B);
            context->p = p;
            break;
        }
            
        // ROL
        case 0x2A:
        case 0x26:
        case 0x36:
        case 0x2E:
        case 0x3E:
        {
            uint8 val = ReadOperandValueForCurrentInstruction(instruction, context);
            bool initialCarry = IsCarrySet(context);
            SetOrClearCarry((val & 0x80) > 0, context);
            val <<= 1;
            val &= 0xFE;
            val |= (initialCarry ? 0x1 : 0x0);
            WriteResultForCurrentInstruction(val, instruction, context);
            UpdateZeroNegative(val, context);
            break;
        }
            
        // ROR
        case 0x6A:
        case 0x66:
        case 0x76:
        case 0x6E:
        case 0x7E:
        {
            uint8 val = ReadOperandValueForCurrentInstruction(instruction, context);
            int initialCarry = IsCarrySet(context);
            SetOrClearCarry((val & 0x01) > 0, context);
            val >>= 1;
            val &= 0x7F;
            val |= (initialCarry ? 0x80 : 0);
            WriteResultForCurrentInstruction(val, instruction, context);
            UpdateZeroNegative(val, context);
            break;
        }
            
        // RTI
        case 0x40:
        {
            uint8 p = PullByte(context);
            p &= ~(0x20 | MCS6502_STATUS_B);
            context->p = p;
            uint8 lo = PullByte(context);
            uint8 hi = PullByte(context);
            context->pc = ((hi << 8) | lo);
            doNotUpdatePC = true;
            break;
        }
            
        // RTS
        case 0x60:
        {
            uint8 lo = PullByte(context);
            uint8 hi = PullByte(context);
            context->pc = ((hi << 8) | lo) + 1;
            doNotUpdatePC = true;
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
            uint8 operand = ReadOperandValueForCurrentInstruction(instruction, context);
            if (!IsDecimalSet(context)) {
                // SBC is apparently exactly equivalent to ADC with the operand as ones-complement inverted, so:
                operand = ~operand;
                //
                unsigned int sum = context->a + operand + (IsCarrySet(context) ? 1 : 0);
                // Overflow flag is set if the twos complement (signed) result is > +127 or < -128.
                // The simplest way for us to do this is just do the addition as signed values and check.
                int vres = (signed char)context->a + (signed char)operand + (IsCarrySet(context) ? 1 : 0);
                context->a = sum & 0xFF;
                SetOrClearCarry(sum > 0xFF, context);
                UpdateZeroNegative(context->a, context);
                SetOrClearOverflow((vres > 127 || vres < -128), context);
            } else {
                int operandInteger = IntegerFromBCD(operand);
                int accInteger = IntegerFromBCD(context->a);
                int difference = accInteger - operandInteger - (IsCarrySet(context) ? 0 : 1);
                if (difference < 0) {
                    ClearCarry(context);
                    difference += 100;
                } else {
                    SetCarry(context);
                }
                context->a = BCDFromInteger(difference);
                UpdateZeroNegative(context->a, context);
                // Don't do anything to overflow flag. It's not documented behavior.
            }
            break;
        }
            
        // SEC
        case 0x38:
        {
            SetCarry(context);
            break;
        }
            
        // SED
        case 0xF8:
        {
            SetDecimal(context);
            break;
        }
            
        // SEI
        case 0x78:
        {
            SetInterruptDisable(context);
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
            uint16 addr = EffectiveOperandAddressForInstruction(instruction, context, NULL);
            MCS6502WriteByte(addr, context->a, context);
            break;
        }
            
        // STX
        case 0x86:
        case 0x96:
        case 0x8E:
        {
            uint16 addr = EffectiveOperandAddressForInstruction(instruction, context, NULL);
            MCS6502WriteByte(addr, context->x, context);
            break;
        }
            
        // STY
        case 0x84:
        case 0x94:
        case 0x8C:
        {
            uint16 addr = EffectiveOperandAddressForInstruction(instruction, context, NULL);
            MCS6502WriteByte(addr, context->y, context);
            break;
        }
            
        // TAX
        case 0xAA:
        {
            context->x = context->a;
            UpdateZeroNegative(context->x, context);
            break;
        }
            
        // TAY
        case 0xA8:
        {
            context->y = context->a;
            UpdateZeroNegative(context->y, context);
            break;
        }
            
        // TSX
        case 0xBA:
        {
            context->x = context->sp;
            UpdateZeroNegative(context->x, context);
            break;
        }
            
        // TXA
        case 0x8A:
        {
            context->a = context->x;
            UpdateZeroNegative(context->a, context);
            break;
        }
            
        // TXS
        case 0x9A:
        {
            context->sp = context->x;
            break;
        }
            
        // TYA
        case 0x98:
        {
            context->a = context->y;
            UpdateZeroNegative(context->a, context);
            break;
        }
    }
    
    // Update PC and timing
    if (!doNotUpdatePC) {
        context->pc = context->pc + LengthForInstruction(instruction);
    }
    
#ifdef PRINT_DEBUG_OUTPUT
    // Dump the context state
    printf("  PC=%04X SP=01%02X A=%02X X=%02X Y=%02X N=%d V=%d D=%d I=%d Z=%d C=%d\n",
           context->pc, context->sp, context->a, context->x, context->y,
           IsNegativeSet(context) ? 1 : 0,
           IsOverflowSet(context) ? 1 : 0,
           IsDecimalSet(context) ? 1 : 0,
           IsInterruptDisableSet(context) ? 1 : 0,
           IsZeroSet(context) ? 1 : 0,
           IsCarrySet(context) ? 1 : 0);
    
    // Dump stack
    if (context->sp < 0xFF) {
        printf("  STACK: ");
        for (uint sp = 0xFF; sp > context->sp; sp--) {
            printf("%02X ", MCS6502ReadByte(0x0100 + sp, context));
        }
        printf("\n");
    }
#endif
    
    // The base timing comes from the instruction data, but it's the final thing
    // we add in here. In the case of branches, the data specifies zero and we
    // have already updated the timing explicitly depending on whether the branch was taken.
    // In several other cases, we will have conditionally added one to the cycle count
    // in certain address modes depending on runtime indirection conditions.
    context->timingForLastOperation += instruction->timing;
    
    if (originalPC == context->pc) {
        return MCS6502ExecResultHalting;
    }

    return MCS6502ExecResultRunning;
}

//
// This is the "data bus": operations to read and write bytes on the bus.
//

static inline uint8 MCS6502ReadByte(uint16 addr, MCS6502ExecutionContext * context)
{
    return context->readByte(addr, context->readWriteContext);
}

static inline void MCS6502WriteByte(uint16 addr, uint8 byte, MCS6502ExecutionContext * context)
{
    context->writeByte(addr, byte, context->readWriteContext);
}

//
// Internal helper functions
//

static inline void PushByte(uint8 byte, MCS6502ExecutionContext * context)
{
    uint16 esp = 0x0100 + context->sp;
    MCS6502WriteByte(esp, byte, context);
    context->sp = context->sp - 1;
}

static inline void PushFlags(bool b, MCS6502ExecutionContext * context)
{
    uint8 p = context->p;
    p |= 0x20; // bit 5 always set in pushed status
    if (b) {
        p |= MCS6502_STATUS_B;
    } else {
        p &= (~MCS6502_STATUS_B);
    }
    PushByte(p, context);
}

static inline uint8 PullByte(MCS6502ExecutionContext * context)
{
    context->sp = context->sp + 1;
    uint16 esp = 0x0100 + context->sp;
    return MCS6502ReadByte(esp, context);
}

static inline uint16 ReadWordAtAddress(uint16 addr, MCS6502ExecutionContext * context)
{
    uint8 veclo = MCS6502ReadByte(addr, context);
    uint8 vechi = MCS6502ReadByte(addr + 1, context);
    return (vechi << 8) | veclo;
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

static void HandleIRQ(MCS6502ExecutionContext * context)
{
    context->irqPending = false;
    uint16 returnPC = context->pc;
    PushByte((returnPC >> 8) & 0xFF, context);
    PushByte(returnPC & 0xFF, context);
    PushFlags(false, context);
    SetInterruptDisable(context);
    context->pc = ReadWordAtAddress(MCS6502_IRQ_BRK, context);
    context->timingForLastOperation = 7;
}

static void HandleNMI(MCS6502ExecutionContext * context)
{
    context->nmiPending = false;
    uint16 returnPC = context->pc;
    PushByte((returnPC >> 8) & 0xFF, context);
    PushByte(returnPC & 0xFF, context);
    PushFlags(false, context);
    SetInterruptDisable(context);
    context->pc = ReadWordAtAddress(MCS6502_NMI, context);
    context->timingForLastOperation = 7;
}

static uint16 EffectiveOperandAddressForInstruction(MCS6502Instruction * instruction, MCS6502ExecutionContext * context, bool * crossesPageBoundary)
{
    #define ADDRESSES_ON_DIFFERENT_PAGES(addr1, addr2) (((addr1) & 0xFF00) != ((addr2) & 0xFF00))
    switch (instruction->mode) {
        case MCS6502AddressingZeroPage:
        {
            return MCS6502ReadByte(context->pc + 1, context);
        }
        case MCS6502AddressingZeroPageX:
        {
            return (uint16)(uint8)(MCS6502ReadByte(context->pc + 1, context) + context->x);
        }
        case MCS6502AddressingZeroPageY:
        {
            return (uint16)(uint8)(MCS6502ReadByte(context->pc + 1, context) + context->y);
        }
        case MCS6502AddressingAbsolute:
        {
            return ReadWordAtAddress(context->pc + 1, context);
        }
        case MCS6502AddressingAbsoluteX:
        {
            uint16 baseAddr = ReadWordAtAddress(context->pc + 1, context);
            uint16 finalAddr = baseAddr + context->x;
            if (crossesPageBoundary != NULL) {
                *crossesPageBoundary = ADDRESSES_ON_DIFFERENT_PAGES(baseAddr, finalAddr);
            }
            return finalAddr;
        }
        case MCS6502AddressingAbsoluteY:
        {
            uint16 baseAddr = ReadWordAtAddress(context->pc + 1, context);
            uint16 finalAddr = baseAddr + context->y;
            if (crossesPageBoundary != NULL) {
                *crossesPageBoundary = ADDRESSES_ON_DIFFERENT_PAGES(baseAddr, finalAddr);
            }
            return finalAddr;
        }
        case MCS6502AddressingIndirect:
        {
            uint8 loIndirect = MCS6502ReadByte(context->pc + 1, context);
            uint8 hiIndirect = MCS6502ReadByte(context->pc + 2, context);
            uint16 indirect = (hiIndirect << 8 | loIndirect);
            // This mode has no carry so force a wrap in the hi address if it crosses
            // a page. User code should never do this because it's bad to, but we
            // will behave like the hardware would just in case.
            uint8 lo = MCS6502ReadByte(indirect, context);
            uint8 hi;
            if (loIndirect == 0xFF) {
                hi = MCS6502ReadByte(hiIndirect << 8, context);
            } else {
                hi = MCS6502ReadByte(indirect + 1, context);
            }
            return (hi << 8 | lo);
        }
        case MCS6502AddressingXIndirect:
        {
            uint8 addr = (uint8)(context->x + MCS6502ReadByte(context->pc + 1, context));
            return ReadWordAtAddress(addr, context);
        }
        case MCS6502AddressingIndirectY:
        {
            uint8 baseAddr = MCS6502ReadByte(context->pc + 1, context);
            uint16 finalAddr = ReadWordAtAddress(baseAddr, context) + context->y;
            if (crossesPageBoundary != NULL) {
                *crossesPageBoundary = ADDRESSES_ON_DIFFERENT_PAGES(baseAddr, finalAddr);
            }
            return finalAddr;
        }
        case MCS6502AddressingRelative:
        {
            signed char offset = (signed char)MCS6502ReadByte(context->pc + 1, context);
            uint16 addr = context->pc + 2 + offset; // all relative instructions are 2 bytes long, and we need to account for that here.
            if (crossesPageBoundary != NULL) {
                *crossesPageBoundary = ADDRESSES_ON_DIFFERENT_PAGES(addr, context->pc);
            }
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
    #undef ADDRESSES_ON_DIFFERENT_PAGES
}

static uint8 ReadOperandValueForCurrentInstruction(MCS6502Instruction * instruction, MCS6502ExecutionContext * context)
{
    if (instruction->mode == MCS6502AddressingImmediate) {
        return MCS6502ReadByte(context->pc + 1, context);
    } else if (instruction->mode == MCS6502AddressingAccumulator) {
        return context->a;
    } else {
        bool crossesPageBoundary = false;
        uint16 addr = EffectiveOperandAddressForInstruction(instruction, context, &crossesPageBoundary);
        if (crossesPageBoundary && instruction->timingAddOne) {
            context->timingForLastOperation += 1;
        }
        return MCS6502ReadByte(addr, context);
    }
}

static void WriteResultForCurrentInstruction(uint8 result, MCS6502Instruction * instruction, MCS6502ExecutionContext * context)
{
   if (instruction->mode == MCS6502AddressingAccumulator) {
        context->a = result;
   } else {
        uint16 addr = EffectiveOperandAddressForInstruction(instruction, context, NULL);
       MCS6502WriteByte(addr, result, context);
   }
}

static int LengthForInstruction(MCS6502Instruction * instruction)
{
    switch (instruction->mode) {
        case MCS6502AddressingImplied:
        case MCS6502AddressingAccumulator:
            return 1;
            
        case MCS6502AddressingImmediate:
        case MCS6502AddressingZeroPage:
        case MCS6502AddressingZeroPageX:
        case MCS6502AddressingZeroPageY:
        case MCS6502AddressingXIndirect:
        case MCS6502AddressingIndirectY:
        case MCS6502AddressingRelative:
            return 2;

        case MCS6502AddressingAbsolute:
        case MCS6502AddressingAbsoluteX:
        case MCS6502AddressingAbsoluteY:
        case MCS6502AddressingIndirect:
            return 3;
        default: return 1;
    }
}

static void ExecuteConditionalBranch(bool condition, MCS6502Instruction * instruction, MCS6502ExecutionContext * context)
{
    if (condition) {
        bool crossesPageBoundary = false;
        context->pc = EffectiveOperandAddressForInstruction(instruction, context, &crossesPageBoundary);
        context->timingForLastOperation += 3;
        if (instruction->timingAddOne && crossesPageBoundary) {
            context->timingForLastOperation += 1;
        }
    } else {
        context->pc = context->pc + LengthForInstruction(instruction);
        context->timingForLastOperation += 2;
    }
}

//
// DEBUG
//

char * DisassembleCurrentInstruction(MCS6502Instruction * instruction, MCS6502ExecutionContext * context)
{
    static char scratch[256];
    switch (instruction->mode) {
        case MCS6502AddressingImmediate:
        {
            uint8 val = MCS6502ReadByte(context->pc + 1, context);
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
            uint8 lo = MCS6502ReadByte(context->pc + 1, context);
            uint8 hi = MCS6502ReadByte(context->pc + 2, context);
            sprintf(scratch, "($%02X%02X)", hi, lo);
            break;
        }
        case MCS6502AddressingAbsolute:
        {
            uint8 lo = MCS6502ReadByte(context->pc + 1, context);
            uint8 hi = MCS6502ReadByte(context->pc + 2, context);
            sprintf(scratch, "$%02X%02X", hi, lo);
            break;
        }
        case MCS6502AddressingZeroPage:
        {
            uint8 addr = MCS6502ReadByte(context->pc + 1, context);
            sprintf(scratch, "$%02X", addr);
            break;
        }
        case MCS6502AddressingAbsoluteX:
        {
            uint8 lo = MCS6502ReadByte(context->pc + 1, context);
            uint8 hi = MCS6502ReadByte(context->pc + 2, context);
            sprintf(scratch, "$%02X%02X, X", hi, lo);
            break;
        }
        case MCS6502AddressingAbsoluteY:
        {
            uint8 lo = MCS6502ReadByte(context->pc + 1, context);
            uint8 hi = MCS6502ReadByte(context->pc + 2, context);
            sprintf(scratch, "$%02X%02X, Y", hi, lo);
            break;
        }
        case MCS6502AddressingZeroPageX:
        {
            uint8 addr = MCS6502ReadByte(context->pc + 1, context);
            sprintf(scratch, "$%02X, X", addr);
            break;
        }
        case MCS6502AddressingZeroPageY:
        {
            uint8 addr = MCS6502ReadByte(context->pc + 1, context);
            sprintf(scratch, "$%02X, Y", addr);
            break;
        }
        case MCS6502AddressingXIndirect:
        {
            uint8 addr = MCS6502ReadByte(context->pc + 1, context);
            sprintf(scratch, "($%02X, X)", addr);
            break;
        }
        case MCS6502AddressingIndirectY:
        {
            uint8 addr = MCS6502ReadByte(context->pc + 1, context);
            sprintf(scratch, "($%02X), Y", addr);
            break;
        }
        case MCS6502AddressingRelative:
        {
            signed char offset = (signed char)MCS6502ReadByte(context->pc + 1, context);
            uint16 addr = context->pc + 2 + offset;
            sprintf(scratch, "$%04X", addr); // this is a special case, it's not really the disassembly
            break;
        }
        case MCS6502AddressingImplied:
        default:
            scratch[0] = '\0';
            break;
    }
    static char line[260];
    sprintf(line, "%s %s", instruction->mnemonic, scratch);
    return line;
}
