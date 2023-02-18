package Minimal_Containers is

   Capacity_Error : exception;

   type Count_Type is range 0 .. 2**31 - 1;

   type Hash_Type is range 0 .. 2**31 - 1;

end Minimal_Containers;
