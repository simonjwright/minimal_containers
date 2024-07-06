The motivation for these containers was use in [ColdFrame](https://simonjwright.github.io/coldframe/), an open-source code generator backend for use with UML tools in a restricted environment (for example, a BBC micro:bit).

In such an environment, it's normal to strip out unused code and data at link time (`-gc-sections` with GNU `ld`, `-dead_strip` with Apple `ld`).

Unfortunately, it turns out that no primitive subprograms of tagged types can be stripped (they are all referenced by the dispatch table).

These containers are still tagged, because (without compiler extensions) ColdFrame expects to use prefixed notation (_object_._primitive subprogram (...)_, as would users. However, the number of subprograms has been much reduced, and support packages aren't needed (except for sorting vectors).

The containers provided are `Bounded_Hashed_Maps` and `Bounded_Vectors` (the ones required by ColdFrame in the Ravenscar version). Both support forward and reverse generalised read-only iteration.

`Bounded_Vectors` don't support constant or variable indexing.
`Bounded_Hashed_Maps` don't have a `Clear` operation.

Minimal\_Containers are compatible with Alire.

## Tampering checks ##

In their unchecked form, both containers would misbehave if an element was deleted during a forward iteration over the container, in that the "next" element would be skipped.

This would be undesirable, so a generation check has been introduced, raising `Program_Error` if this is detected.

Unlike Maps, Vectors support both forward and reverse iteration; if it's necessary to delete some elements from a Vector, e.g. those that have become invalid, do it in a reverse loop over the index,
```
for Index in reverse Vector.First_Index .. Vector.Last_Index loop
   if Is_Invalid (Vector.Element (Index)) then
      Vector.Delete (Index);
   end if;
end loop;
```

## Experimental results ##

### [Microbit](https://github.com/simonjwright/coldframe/tree/master/examples/microbit) ###

The "standard" containers are those provided as part of [Cortex GNAT RTS](https://github.com/simonjwright/cortex-gnat-rts); the "minimal" containers are these.

The builds used GCC 13.0.1 (20230129).
Using the "minimal" containers resulted in a reduction of 29% (40788 bytes) in the `text` size.

| Containers | text | data | bss |
| :--------- | ---: | ---: | --: |
| standard | 139380 | 744 | 1684 |
| minimal | 98592 | 824 | 1700 |

### [STM32F4](https://github.com/simonjwright/coldframe/tree/master/examples/stm32f4) ###

#### With gnat\_arm\_elf ####

The "embedded" containers are those provided in [Alire](https://alire.ada.dev/docs/#introduction) by the `gnat_arm_elf 2.2.1` compiler as `embedded-stm32f4`.

The  builds used the `gnat_arm_elf 2.2.1` compiler, with binder switches `-minimal`, `-d512` (default primary stack size), `-D128` (default secondary stack size) and `-Q0` (meant to say don't generate any secondary stacks, though I don't see how they'd be used, but in fact appears to generate one).
Using the "minimal" containers resulted in a reduction of 27% (83764 bytes) in the `text` size.

| Containers | text | data | bss |
| :--------- | ---: | ---: | --: |
| embedded | 305408 | 4244 | 32384 |
| minimal | 221644 | 4124 | 32408 |

#### With Cortex GNAT RTS ####

The builds used GCC 13.0.1 (20230129), with binder switch `-minimal`. The "standard" containers are those from Cortex GNAT RTS, based on the GCC 4.9.1 version.
Using the "minimal" containers resulted in a reduction of 22% (37156 bytes) in the `text` size.

| Containers | text | data | bss |
| :--------- | ---: | ---: | --: |
| standard | 166956 | 1352 | 3768 |
| minimal | 129800 | 1432 | 3800 |

<!--
Having converted the event queue to use minimal vectors (3 kinds,
5 instances) we get 94128/1432/3816

and again 94303/1432/3816
-->
