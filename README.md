The motivation for these containers was use in [ColdFrame](https://simonjwright.github.io/coldframe/), an open-source code generator backend for use with UML tools in a restricted environment (a BBC micro:bit).

In such an environment, it's normal to strip out unused code and data at link time (`-gc-sections` with GNU `ld`, `-dead_strip` with Apple `ld`).

Unfortunately, it turns out that no primitive subprograms of tagged types can be stripped (they are all referenced by the dispatch table).

These containers are still tagged, because (without compiler extensions) ColdFrame expects to use prefixed notation (_object_._primitive subprogram (...)_, as would users. However, the number of subprograms has been much reduced, and support packages aren't needed.

The containers provided are `Bounded_Hashed_Maps` and `Bounded_Vectors` (the ones required by ColdFrame in the Ravenscar version). Both support forward and reverse generalised read-only iteration.

At present (version 0.1.2-dev), neither container checks for tampering; deleting an element during iteration works fine so long as it's a _reverse_ iteration.

Minimal\_Containers are compatible with Alire.

## Experimental results ##

### [Microbit](https://github.com/simonjwright/coldframe/tree/master/examples/microbit) ###

The "standard" containers are those provided as part of [Cortex GNAT RTS](https://github.com/simonjwright/cortex-gnat-rts); the "minimal" containers are these.

The builds used GCC 13.0.1 (20230129).

| Containers | text | data | bss |
| :--------- | ---: | ---: | --: |
| standard | 139380 | 744 | 1684 |
| minimal | 98592 | 824 | 1700 |

### [STM32F4](https://github.com/simonjwright/coldframe/tree/master/examples/stm32f4) ###

The "embedded" containers are those provided in [Alire](https://alire.ada.dev/docs/#introduction) by the `gnat_arm_elf 2.2.1` compiler as `embedded-stm32f4`.

The builds used the `gnat_arm_elf 2.2.1` compiler (there were compiler issues both with GCC 13.0.1 and [GCC 12.2.0](https://github.com/simonjwright/distributing-gcc/releases/tag/gcc-12.2.0-arm-eabi)).

| Containers | text | data | bss |
| :--------- | ---: | ---: | --: |
| embedded | 305408 | 4244 | 32384 |
| minimal | 221612 | 4124 | 32408 |
