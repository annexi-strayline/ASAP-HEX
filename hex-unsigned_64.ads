------------------------------------------------------------------------------
--                                                                          --
--                   Generic HEX String Handling Package                    --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai, Ensi Martini, Aninda Poddar, Noshen Atashe               --
--    (ANNEXI-STRAYLINE)                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

-- Standard, verified package to represent an 64-bit modular value

with Interfaces;
with Hex.Modular_Codec;

package Hex.Unsigned_64
  with SPARK_Mode
is
   pragma Assertion_Policy (Pre    => Check,
                            Post   => Ignore,
                            Assert => Ignore);
   
   subtype Unsigned_64 is Interfaces.Unsigned_64;
   use type Unsigned_64;
   
   package Codec is new Hex.Modular_Codec
     (Modular_Value => Unsigned_64,
      Bit_Width     => 64);
   
   
   Maximum_Length: Positive renames Codec.Max_Nibbles;
   
   function Decode (Input: String) return Unsigned_64
     renames Codec.Decode;
   
   procedure Decode (Input           : in     String;
                     Value           :    out Unsigned_64)
     renames Codec.Decode;
   
   
   function  Encode (Value: Unsigned_64; Use_Case: Set_Case := Lower_Case)
                    return String
     renames Codec.Encode;
   
   
   procedure Encode (Value   : in     Unsigned_64; 
                     Buffer  :    out String;
                     Use_Case: in     Set_Case := Lower_Case)
     renames Codec.Encode;
   
end Hex.Unsigned_64;
