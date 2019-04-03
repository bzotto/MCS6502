# MCS6502

This is a simple 6502 emulator core. It was originally built as the basis of an Apple II emulator project, but is presented here as a   functional emulator for the 8-bit CPU in one C module. Suitable for dropping into whatever your project is!

## Getting started 

The emulator can run as a black box. You need to provide a "data bus" to let it read and write data. The whole emulator state is represented by a structure of type `MCS6502ExecutionContext` that you can peer into between ticks if you'd like to see the CPU state. When you initialize this context, you need to provide it with one function for reading bytes and one for writing bytes. This is the "data bus" for the CPU.

    MCS6502ExecutionContext context;
    MCS6502Init(&context, readBytesFunction, writeBytesFunction, NULL);  // Final param is optional context.
    
To simulate the CPU doing a power-on-reset, the next thing you do is tell it to reset:

    MCS6502Reset(&context);
    
And now it's ready to actually run, and you have **two choices**. If you're running it in a standard timed loop, representing a fixed clock speed for the CPU, then you want to repeatedly call "tick", and the per-instruction cycle counts will be accurate:

    MCS6502Tick(&context);

However, if you don't care about instruction timing and you just want to run the thing as fast as it will go, you can bypass the timing and instead loop on:

    MCS6502ExecNext(&context);

And that's pretty much it to get it running! (See the header file for return values for these last two functions.)

## But how does this "CPU" actually do anything?

Well, no CPU is an island. Without memory and code to run, there's nothing for it to do. You need to provide two simple functions to the init method above. One returns a byte of data when given an address, and the other writes a byte of data when given an address. That's the only I/O you really need to make the thing work. If you're here, you probably understand already that this is how you map various bits of RAM and/or ROM to the 16-bit address space. **Note** that the final argument to init is a `void *` "context" pointer. This is optional, and whatever you pass into init here will get passed back into your read and write functions later. You can use this to integrate this module with an object oriented system or to support multiple CPU instances, etc.

When the 6502 starts up it will load an address vector from 0xFFFC/0xFFFD, set its program counter to that address, and then start fetching instructions from there. That means at a bare minimum you need to return useful data when asked for values at those addresses and return some code when the CPU starts asking for bytes at the address in the vector. The vectors are defined in the header file for your convenience. 

In practice, in a real hardware configuration, a system will typically tie those high addresses to a ROM chip where the reset vector is hard-coded and points to an address also within ROM where the system start up code is.

You shoud know that the 6502 treats the first "zero page" of 256 bytes (0x00xx) as a special quick-access RAM work area and the second page (0x01xx) as the stack. The rest is up to you. The 6502 was super popular in real life and remains super popular in emulation, so there are great detailed resources all over the place for learning more.

## What it does

- Passes the full [functional test suite here](https://github.com/Klaus2m5/6502_65C02_functional_tests)
- Includes decimal mode in arithmetic, if you're a crazy person who uses that.
- Includes working `ROR` and `ROL` instructions (the very original 6502s did not).
- It's instruction cycle count accurate, if you want that.
- Has been used at the core of a simple Apple II. YMMV in other emulated systems, but definitely let me know if you find something about the CPU not working as you think it should.

## What it doesn't do (sharp corners)

- The emulated CPU is an (original) NMOS 6502, not a 65C02, and has the instruction set and undefined behaviors of that CPU. Undefined behaviors in then emulator will not in all cases reflect the real hardware's undefined behaviors. No "undocumented" opcodes are supported. 

- Although the instruction relative cycle counts are accurate (when using the tick function to execute), this is a *functional emulator*, not a *hardware simulator*. The emulator does not step through the hardware's logical cycle stages to do its reads and writes. All actual work for an instruction is performed on its first tick. Thus you cannot use it as a component of a more complex hardware-signal emulator.

- There are some cases where the CPU will re-read parts of instructions or indirect address base values multiple times during execution of a single instruction, where real hardware would not. (Again, emulator not simulator.) Generally this should not cause any unexpected behavior, although in *theory* I guess that if you were doing something crazy like using an indirected base address as some sort of hardware I/O port... ¯\\_(ツ)_/¯ (But if you do that, your "hardware" design is either insane or brilliant and you should be using a hyper-accurate emulator or a real chip.)

## Interrupts

IRQ and NMI signals are supported by calling `MCS6502IRQ` or `MCS6502NMI` respectively. Doing this will trigger the handler at the standard vectors for these after the current instruction has completed, which is how the real hardware would do it. Just like in real hardware, an IRQ will be ignored if the interrupt-disable flag is set, but an NMI will not. 

Reset of course is supported as seen above in the power-on example. But some computers would tie a physical Reset button in to the CPU's reset line too, and if you want to simulate that you can call that function while the machine is running, too. 

## Debugging

There's a compile-time flag at the top of `MCS6502.c` called `PRINT_DEBUG_OUTPUT`. It's commented-out by default, but if you uncomment it, you'll get printf output to stdout with disassembly, register and stack state as it executes. This will slow the whole thing way down but is handy for debugging.
