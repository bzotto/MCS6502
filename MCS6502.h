//
//  MCS6502.h
//
//  Copyright (c) 2019 by Ben Zotto
//
//  Usage is subject to the MIT License, and copyright credit must be noted
//  in the documenation of binary redistributions. See accompanying LICENSE.TXT.
//

#ifndef MCS6502_h
#define MCS6502_h

// Types
#include <stdio.h>
#include <stdbool.h>
#ifndef uint8
typedef unsigned char uint8;
#endif
#ifndef uint16
typedef unsigned short uint16;
#endif

//
// 6502 Execution context, including CPU state
//

struct _MCS6502ExecutionContext;
typedef uint8 (*MCS6502DataReadByteFunction)(uint16 addr, void * readWriteContext);
typedef void (*MCS6502DataWriteByteFunction)(uint16 addr, uint8 byte, void * readWriteContext);

typedef struct _MCS6502ExecutionContext {
    // CPU state (you can inspect this to debug):
    uint8 a;        // Accumulator
    uint8 x, y;     // X and Y registers
    uint8 sp;       // Stack pointer
    uint16 pc;      // Program counter
    uint8 p;        // Processor status
    
    // These fields are used to track state related to tick-based emulation:
    unsigned int pendingTiming;
    unsigned int timingForLastOperation;
    struct {
        bool irqPending : 1;
        bool nmiPending : 1;
    };
    
    // The "data bus" for the CPU is represented by these function pointers.
    // The optional data access "context" pointer is passed back into the read
    // and write functions which may be useful in identifying the CPU instance being used.
    MCS6502DataReadByteFunction readByte;
    MCS6502DataWriteByteFunction writeByte;
    void * readWriteContext;
} MCS6502ExecutionContext;

//
// Bit flags defined within in the processor status register (p).
// The "b" (break) flag is not valid within the p register, but is
// valid when you push p (PHP) to stack and examine the result directly.
// The unspecified bit 5 of the status is unused and undefined.
//

#define MCS6502_STATUS_N  0x80
#define MCS6502_STATUS_V  0x40
#define MCS6502_STATUS_B  0x10
#define MCS6502_STATUS_D  0x08
#define MCS6502_STATUS_I  0x04
#define MCS6502_STATUS_Z  0x02
#define MCS6502_STATUS_C  0x01

//
// Execution status results. "Running" is the normal state. "Halting" tells you
// that the CPU has branched back to the same PC address, meaning it's in a loop.
// (A halt loop isn't necessary wrong or bad; the CPU may be waiting for an IRQ/NMI.)
// Invalid operation is returned when the opcode at the PC is not a known
// instruction.
//

typedef enum _MCS6502ExecResult {
    MCS6502ExecResultRunning,
    MCS6502ExecResultHalting,
    MCS6502ExecResultInvalidOperation
} MCS6502ExecResult;

// Initialize an execution context by passing a pointer to a context structure
// along with (required) function pointers to the read and write "data bus" functions,
// and (optional) some value that you wish to have passed back with reads and writes.

void
MCS6502Init(
    MCS6502ExecutionContext * context,
    MCS6502DataReadByteFunction readByteFn,
    MCS6502DataWriteByteFunction writeByteFn,
    void * readWriteContext
);

// The three useful hardware interrupt triggers. Call MCS6502Reset() after
// MCS6502Init() and before a MCS6502Tick() to perform power-on reset.

void
MCS6502Reset(
    MCS6502ExecutionContext * context
);
void
MCS6502IRQ(
    MCS6502ExecutionContext * context
);
void
MCS6502NMI(
    MCS6502ExecutionContext * context
);

// The core of the emulation is calling the tick function in a timed loop. Each
// tick does the "work" of a clock cycle. The emulator is essentially timing-correct
// on this basis in terms of execution of the instruction stream. But note it does
// all work for an instruction on that instruction's first "tick" and then
// will decrement its timing count before starting the next instruction. Because
// it does not split up reads and writes into hardware-accurate cycles, it cannot
// be used to simulate cycle-precise hardware access.

MCS6502ExecResult
MCS6502Tick(
    MCS6502ExecutionContext * context
);

//
// If you aren't running the CPU on a fixed clock that simulates real operation
// timing, then just call the core exec function below instead of tick. There will
// be no "wait" cycles; you'll get work done with each call. It's not meaningful to
// mix the two approaches.
//

MCS6502ExecResult
MCS6502ExecNext(
    MCS6502ExecutionContext * context
);

//
// Important vector locations
//

#define MCS6502_NMI_LO      0xFFFA
#define MCS6502_NMI_HI      0xFFFB
#define MCS6502_NMI         MCS6502_NMI_LO

#define MCS6502_RESET_LO    0xFFFC
#define MCS6502_RESET_HI    0xFFFD
#define MCS6502_RESET       MCS6502_RESET_LO

#define MCS6502_IRQ_BRK_LO  0xFFFE
#define MCS6502_IRQ_BRK_HI  0xFFFF
#define MCS6502_IRQ_BRK     MCS6502_IRQ_BRK_LO

#endif /* MCS6502_h */
