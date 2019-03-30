//
//  MCS6502.h
//
//  Created by Ben Zotto on 3/19/19.
//  Copyright © 2019 Ben Zotto. All rights reserved.
//

#ifndef MCS6502_h
#define MCS6502_h

#include <stdio.h>
#include <stdbool.h>

// Types

#ifndef uint8
typedef unsigned char uint8;
#endif
#ifndef uint16
typedef unsigned short uint16;
#endif

//
// 6502 CPU state
//

typedef struct _MCS6502CPU {
    uint8 a;        // Accumulator
    uint8 x, y;     // X and Y registers
    uint8 sp;       // Stack pointer
    uint16 pc;      // Program counter
    uint8 p;        // Processor status
    
    // These fields are used to track state related to tick-based emulation:
    unsigned int timing;
    struct {
        bool irqPending : 1;
        bool nmiPending : 1;
    };
} MCS6502CPU;

//
// Bit flags defined within in the processor status register (p).
//

#define MCS6502_STATUS_N  0x80
#define MCS6502_STATUS_V  0x40
#define MCS6502_STATUS_B  0x10
#define MCS6502_STATUS_D  0x08
#define MCS6502_STATUS_I  0x04
#define MCS6502_STATUS_Z  0x02
#define MCS6502_STATUS_C  0x01

//
// Structure containing function pointers for reading and writing values
// from addressable locations. The context pointer is an arbitrary value
// you can provide which will be provided back to you as a function
// argument (this allows you to optionally differentiate between CPU
// instances).
//

typedef struct _MCS6502DataBus {
    void * context;
    uint8 (*readByte)(uint16 addr, void * context);
    void (*writeByte)(uint16 addr, uint8 byte, void * context);
} MCS6502DataBus;

//
//
//

typedef enum _MCS6502ExecResult {
    MCS6502ExecResultPending,
    MCS6502ExecResultCompleted,
    MCS6502ExecResultHalting,
    MCS6502ExecResultInvalidOperation
} MCS6502ExecResult;

// Initialize a CPU structure
void MCS6502Init(MCS6502CPU * cpu);

// The three useful hardware interrupt triggers. Call MCS6502Reset() after
// MCS6502Init() and before a MCS6502Tick() to do power-on reset.
void MCS6502Reset(MCS6502CPU * cpu, MCS6502DataBus * dataBus);
void MCS6502IRQ(MCS6502CPU * cpu, MCS6502DataBus * dataBus);
void MCS6502NMI(MCS6502CPU * cpu, MCS6502DataBus * dataBus);

MCS6502ExecResult MCS6502Tick(MCS6502CPU * cpu, MCS6502DataBus * dataBus);

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
