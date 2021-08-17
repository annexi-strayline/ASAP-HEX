------------------------------------------------------------------------------
--                                                                          --
--                   Generic HEX String Handling Package                    --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2018-2019, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

generic
   type Modular_Value is mod <>;
   Bit_Width: in Positive;
   -- By requiring the user to provide the Bit_Width, which is then
   -- verified by an Assertion, we avoid needing to do some special
   -- math at elaboration (which could be dynamic), and we also
   -- make the proof much easier. To be fair, this is not a big ask!
   
package Hex.Modular_Codec 
  with SPARK_Mode => On
is
   -- ** Precondition Warning **
   -- For non-SPARK users, it is recommended that this package is
   -- instantiated in a region where the Assertion_Policy has Pre and Assertion
   -- set to Check.
   pragma Assertion_Policy (Post => Ignore);
   
   pragma Assert ((Bit_Width mod 4) = 0);
   pragma Assert (2 ** Bit_Width = Modular_Value'Modulus);
   
   -- This avoids some very serrious maths!
   
   Max_Nibbles: constant Positive := Bit_Width / 4;
   
   ------------
   -- Decode --
   ------------
   function  Decode (Input: String) return Modular_Value
   with
     Global  => null,
     
     Pre     => Valid_Hex_String (Input)
                and Input'Length <= Max_Nibbles,
     
     Post    => (if Decode'Result = 0 then
                   (for all Digit of Input => Digit = '0')
                 else
                   (for some Digit of Input => Digit /= '0'));
   
   
   procedure Decode (Input: in     String;
                     Value:    out Modular_Value)
   with 
     Global  => null,
     Depends => (Value => Input),
     
     Pre     => Valid_Hex_String (Input)
                and Input'Length <= Max_Nibbles,
     
     Post    => (if Value = 0 then
                   (for all Digit of Input => Digit = '0')
                 else
                   (for some Digit of Input => Digit /= '0'));
   
   -- Decodes an encoded value into the corresponding modular value.
   -- Input shall be a Valid_Hex_String. Such strings can be identified
   -- from any arbitrary string via the Identify_Hex_String operation
   
   
   ------------
   -- Encode --
   ------------
   function  Encode (Value: Modular_Value; Use_Case: Set_Case := Lower_Case)
                    return String
   with
     Global => null,
     
     Post   => Valid_Hex_String (Encode'Result)
               
               and
               Encode'Result'Length <= Max_Nibbles
               
               and
               (if Value = 0 then
                  (for all Digit of Encode'Result => Digit = '0')
                else
                  (for some Digit of Encode'Result => Digit /= '0'))
               
               and 
               (case Use_Case is
                   when Lower_Case =>
                     (for all Digit of Encode'Result 
                        => Digit in '0' .. '9' | 'a' .. 'f'),
                   when Upper_Case =>
                     (for all Digit of Encode'Result 
                        => Digit in '0' .. '9' | 'A' .. 'F'));
   -- Encodes Value into the shortest Hex_String representation,
   -- if Lower_Case is true, all alphabetical digits will be encoded as
   -- lower case, otherwise all alphabetical digits will be encoded as upper
   -- case
   
   
   procedure Encode (Value   : in     Modular_Value; 
                     Buffer  :    out String;
                     Use_Case: in     Set_Case := Lower_Case)
   with
     Global  => null,
     Depends => (Buffer => (Value, Buffer, Use_Case)),
     
     Pre  => Buffer'Length >= Max_Nibbles,
     
     Post => Valid_Hex_String (Buffer)
             
             and
             (if Value = 0 then
                (for all Digit of Buffer => Digit = '0')
              else
                (for some Digit of Buffer => Digit /= '0'))
             
             and
             (case Use_Case is
                 when Lower_Case =>
                   (for all Digit of Buffer 
                      => Digit in '0' .. '9' | 'a' .. 'f'),
                 when Upper_Case =>
                   (for all Digit of Buffer 
                      => Digit in '0' .. '9' | 'A' .. 'F'))
             
             and
             (if Buffer'Length > Max_Nibbles then
                (for all I in Buffer'First .. (Buffer'Last - Max_Nibbles)
                   => Buffer(I) = '0'));
   
   -- Places the Hex-encoded version of Value into the Buffer, setting
   -- any unfilled positions to zero.
   --
   -- Buffer shall be at least long enough to contain the maximum value of
   -- Modular_Value
   --
   -- If Lower_Case is true, all alphabetical digits will be encoded as
   -- lower case, otherwise all alphabetical digits will be encoded as upper
   -- case
   
end Hex.Modular_Codec;

   
   
