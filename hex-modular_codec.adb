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

package body Hex.Modular_Codec 
  with SPARK_Mode => On
is
   
   pragma Assertion_Policy (Ignore);
   
   pragma Suppress (Division_Check);
   pragma Suppress (Index_Check);
   pragma Suppress (Length_Check);
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);
   pragma Suppress (Tag_Check);
   -- Package fully verified
   
   --
   -- Proof Support
   --
   
   type Nibble is mod 2**4;
   
   subtype Encoded_Digits is String (1 .. Max_Nibbles);
   type    Decoded_Digits is array (Positive range 1 .. Max_Nibbles) of Nibble;
   -- Decoded_Digits (like Encoded_Digits) is in Big Endian order, with
   -- the most significant nibble at position 'First. This greatly simplifies 
   -- proof!
   
   
   -- Table-based Decode/Encode for Nibbles
   type Encode_Table is array (Nibble) of Hex_Character;
   
   Nibble_Encode_Lower: constant Encode_Table
     := (0  => '0', 1  => '1', 2  => '2', 3  => '3',
         4  => '4', 5  => '5', 6  => '6', 7  => '7',
         8  => '8', 9  => '9', 10 => 'a', 11 => 'b',
         12 => 'c', 13 => 'd', 14 => 'e', 15 => 'f');
   
   Nibble_Encode_Upper: constant Encode_Table
     := (0  => '0', 1  => '1', 2  => '2', 3  => '3',
         4  => '4', 5  => '5', 6  => '6', 7  => '7',
         8  => '8', 9  => '9', 10 => 'A', 11 => 'B',
         12 => 'C', 13 => 'D', 14 => 'E', 15 => 'F');
   
   
   function Nibble_Decode (Digit: in Hex_Character) return Nibble
   with Inline => True,
     Post   => Nibble_Encode_Lower (Nibble_Decode'Result) = Digit
               or else
               Nibble_Encode_Upper (Nibble_Decode'Result) = Digit;

   
   ----------------------------------------------------------------------------
   function Nibble_Decode (Digit: in Hex_Character) return Nibble is
   begin
      return (case Digit is
                 when '0'       => 0,
                 when '1'       => 1,
                 when '2'       => 2,
                 when '3'       => 3,
                 when '4'       => 4,
                 when '5'       => 5,
                 when '6'       => 6,
                 when '7'       => 7,
                 when '8'       => 8,
                 when '9'       => 9,
                 when 'a' | 'A' => 16#A#,
                 when 'b' | 'B' => 16#B#,
                 when 'c' | 'C' => 16#C#,
                 when 'd' | 'D' => 16#D#,
                 when 'e' | 'E' => 16#E#,
                 when 'f' | 'F' => 16#F#);
   end Nibble_Decode;
   
   ------------------
   -- Batch_Decode --
   ------------------
   -- Decode each nibble for a String/Decoded_Digits pair
   procedure Batch_Decode (Input: in     Encoded_Digits;
                           Batch: in out Decoded_Digits)
   with 
     Global  => null,
     Depends => (Batch => (Input, Batch)),
     Pre     => Valid_Hex_String (Input)
                and (for all N of Batch => N = 0),
     Post    => (for all I in Input'Range
                   => Batch(I) = Nibble_Decode (Input(I)))
       
                and (for all I in Input'Range
                       => (Nibble_Encode_Lower(Batch(I)) = Input(I)
                           or else 
                           Nibble_Encode_Upper(Batch(I)) = Input(I)))
                
                and (if (for all N of Batch => N = 0) then
                       (for all I of Input => I = '0')
                     else
                       (for some I of Input => I /= '0')),
     Inline  => True
   is 
      Zero: Boolean := True
        with Ghost => True;
   begin
      for Digit in Input'Range loop
         Batch(Digit) := Nibble_Decode (Input (Digit));
         
         Zero := Zero and (Input(Digit) = '0');
         
         pragma Loop_Invariant (Valid_Hex_String (Input));
         
         pragma Loop_Invariant
           (for all I in Input'First .. Digit 
              => Batch(I) = Nibble_Decode (Input (I)));
         
         pragma Loop_Invariant
           (for all I in Input'First .. Digit
              => (Nibble_Encode_Lower(Batch(I)) = Input(I)
                 or else
                 Nibble_Encode_Upper(Batch(I)) = Input(I)));
         
         
         pragma Loop_Invariant 
           (if Zero then
              ((for all N of Batch(Input'First .. Digit)
                  => N = 0)
                 and
                (for all I of Input(Input'First .. Digit)
                   => I = '0'))
            else
              ((for some N of Batch(Input'First .. Digit)
                  => N > 0)
                 and
                (for some I of Input(Input'First .. Digit)
                   => I /= '0')));
         
      end loop;
      
   end Batch_Decode;

   
   ------------------
   -- Batch_Encode --
   ------------------
   -- Decode each nibble for a String/Decoded_Digits pair
   procedure Batch_Encode (Batch   : in     Decoded_Digits;
                           Output  : in out Encoded_Digits;
                           Use_Case: in     Set_Case)
   with
     Global => null,
     Depends => (Output => (Batch, Output, Use_Case)),
     Pre  => (for all Digit of Output => Digit = '0'),
     Post => Valid_Hex_String (Output)
             
             and
             (case Use_Case is 
                 when Lower_Case =>
                   (for all C of Output => C in '0' .. '9' | 'a' .. 'f'),
                 when Upper_Case =>
                   (for all C of Output => C in '0' .. '9' | 'A' .. 'F'))
             
             and
             (if (for all N of Batch => N = 0) then
                (for all I of Output => I = '0')
              else
                ((for some N of Batch => N > 0)
                 and
                (for some I of Output => I /= '0'))),
     
     Inline => True
   is 
      Zero: Boolean := True
        with Ghost => True;
   begin
      
      for Digit in Batch'Range loop
         
         Zero := (Zero and Batch(Digit) = 0);
         
         case Use_Case is
            when Upper_Case => 
               Output(Digit) 
                 := Nibble_Encode_Upper (Batch (Digit));
            when Lower_Case => 
               Output(Digit) 
                 := Nibble_Encode_Lower (Batch (Digit));
         end case;
         
         pragma Loop_Invariant (Valid_Hex_String (Output));
         pragma Loop_Invariant 
           (if Zero then
              (for all C of Output => C = '0')
               and
              (for all I in Batch'First .. Digit => Batch(I) = 0)
            else
              (for some C of Output => C /= '0')
               and
              (for some N of Batch => N > 0));
         
         pragma Loop_Invariant 
           (case Use_Case is
               when Lower_Case =>
                 (for all C of Output => C in '0' .. '9' | 'a' .. 'f'),
               when Upper_Case =>
                 (for all C of Output => C in '0' .. '9' | 'A' .. 'F'));
         pragma Loop_Invariant
           (for all I in Batch'First .. Digit 
              => Batch(I) = Nibble_Decode (Output (I)));
      end loop;
      
   end Batch_Encode;
   
   
   -------------------
   -- Batch_Combine --
   -------------------
   -- Convert a given Decoded_Digits set into a Modular_Value
   function Batch_Combine (Batch: Decoded_Digits) return Modular_Value
   with 
     Global => null,
     Pre    => Batch'Length > 1,
     Post   => (if Batch_Combine'Result = 0 then
                  (for all N of Batch => N = 0)
                else
                  (for some N of Batch => N > 0)),
     Inline => True
   is 
      Value_Tracker: array (Batch'Range) of Modular_Value
        := (others => 0)
        with Ghost => True;
      
   begin
      
      pragma Assert (for all N of Batch => N in 0 .. 16#F#);
      
      return Value: Modular_Value := Modular_Value (Batch(Batch'First)) do
         
         Value_Tracker(Batch'First) := Modular_Value (Batch(Batch'First));
         
         for I in Batch'First + 1 .. Batch'Last loop
            
            Value := Value * 16#10# + Modular_Value (Batch(I));
            Value_Tracker(I) := Value;
            
            pragma Loop_Invariant (Value = Value_Tracker(I));
            pragma Loop_Invariant (Modular_Value (Batch(I)) 
                                     in 16#0# .. 16#F#);
            
            pragma Loop_Invariant
              (for all K in Batch'First + 1 .. I =>
                 (Value_Tracker(K) 
                    = (Value_Tracker(K - 1) * 16#10#) 
                    + Modular_Value (Batch(K))));

            
            pragma Loop_Invariant
              (if Value = 0 then
                 (for all K in Batch'First .. I => Batch(K) = 0));
            
            pragma Loop_Invariant
              (if Value > 0 then
                 (for some K in Batch'First .. I => Batch(K) > 0));

         end loop;
      end return;
   end Batch_Combine;
   
   
   ------------------
   -- Batch_Expand --
   ------------------
   -- Convert a given Modular_Value to a batch of Decoded_Digits
   function Batch_Expand (Value: Modular_Value) return Decoded_Digits
   with
     Global => null,
     Post   => (if Value = 0 then
                  (for all N of Batch_Expand'Result => N = 0)
                else
                  (for some N of Batch_Expand'Result => N > 0)),
     Inline => True
   is 
      Dissipator: Modular_Value := Value;
   begin
      return Batch: Decoded_Digits do
         for I in reverse Batch'Range loop
            Batch(I) := Nibble (Dissipator and 16#F#);
            Dissipator := Dissipator / 16#10#;
         end loop;
      end return;
   end Batch_Expand;
   
   
   --
   -- Package Interface
   --
   
   ------------
   -- Decode --
   ------------
   procedure Decode (Input: in     String;
                     Value:    out Modular_Value)
   is 
      Temp_Decoded: Decoded_Digits := (others => 0);
      
      Input_Offset: constant Natural := Input'First - 1
        with Ghost => True;
      -- How much to subtract to get the "start at 1" equivilent.
      -- This is used to compare each position of Input with
      -- Temporary_Decoded
      
   begin
      -- Precondition requires that Input is no longer than
      -- the maximum length, but it may be shorter
      if Input'Length < Max_Nibbles then
         -- We need to make an intermediate to fill it in
         declare
            Temp_Encoded: Encoded_Digits := (others => '0');
            MSN: constant Positive
              := Temp_Encoded'Last - Input'Length + 1;
            
         begin
            Temp_Encoded (MSN .. Temp_Encoded'Last) := Input;
            
            pragma Assert 
              (Temp_Encoded(MSN .. Temp_Encoded'Last) = Input
                 and
                 (for all I of Temp_Encoded(Temp_Encoded'First .. MSN - 1)
                    => I = '0'));
            
            Batch_Decode (Input => Temp_Encoded,
                          Batch => Temp_Decoded);
            
            pragma Assert (for all I in Temp_Encoded'Range
                             => Temp_Decoded(I) 
                               = Nibble_Decode (Temp_Encoded(I)));
            
            Value := Batch_Combine (Temp_Decoded);
            
            pragma Assert (if Value = 0 then
                             (for all I of Temp_Encoded => I = '0')
                           else
                             (for some I of Temp_Encoded => I /= '0')
                              and
                              Value > 0);
            
         end;
      else
         pragma Assert (Input'Length in 1 .. Max_Nibbles);
         
         Batch_Decode (Input => Input,
                       Batch => Temp_Decoded);
        
         pragma Assert (for all I in Input'Range
                          => Temp_Decoded(I - Input_Offset) 
                            = Nibble_Decode (Input(I)));
         
         
         Value := Batch_Combine (Temp_Decoded);
         
         pragma Assert (if Value = 0 then
                             (for all I of Input => I = '0')
                           else
                             (for some I of Input => I /= '0')
                              and
                              Value > 0);
      end if;
      
   end Decode;
   
   ----------------------------------------------------------------------------
   function Decode (Input: String) return Modular_Value is
   begin
      return Value: Modular_Value do
         Decode (Input => Input,
                 Value => Value);
      end return;
   end Decode;
   
   
   ------------
   -- Encode --
   ------------
   procedure Encode (Value   : in     Modular_Value; 
                     Buffer  :    out String;
                     Use_Case: in     Set_Case := Lower_Case)
   is
      Temp_Encoded: Encoded_Digits := (others => '0');
      Temp_Decoded: Decoded_Digits := Batch_Expand (Value);
   begin
      Batch_Encode (Batch    => Temp_Decoded,
                    Output   => Temp_Encoded,
                    Use_Case => Use_Case);
      
      pragma Assert (if Value = 0 then
                       (for all I of Temp_Encoded => I = '0')
                     else
                       (for some I of Temp_Encoded => I /= '0'));
                       
      
      -- Precondition says buffer is at least as long as Temp_Encoded
      if Buffer'Length > Temp_Encoded'Length then
         
         -- Zero extend
         Buffer := (others => '0');
         
         Buffer(Buffer'Last - Temp_Encoded'Length + 1 .. Buffer'Last) 
           := Temp_Encoded;
         
         pragma Assert (Buffer(Buffer'Last - Temp_Encoded'Length + 1
                                 .. Buffer'Last) = Temp_Encoded);
         
         
         pragma Assert (if Value = 0 then
                          (for all Digit of Buffer => Digit = '0')
                        else
                          (for some Digit of Buffer => Digit /= '0'));
         
      else
         
         Buffer := Temp_Encoded;

         pragma Assert (Buffer = Temp_Encoded);
         
         pragma Assert (if Value = 0 then
                          (for all Digit of Buffer => Digit = '0')
                        else
                          (for some Digit of Buffer => Digit /= '0'));
      end if;
      
      pragma Assert (Valid_Hex_String (Buffer));
      
      pragma Assert ((if Value = 0 then
                        (for all Digit of Buffer => Digit = '0')
                      else
                        (for some Digit of Buffer => Digit /= '0')));
      
      pragma Assert ((case Use_Case is
                         when Lower_Case =>
                           (for all Digit of Buffer 
                              => Digit in '0' .. '9' | 'a' .. 'f'),
                         when Upper_Case =>
                           (for all Digit of Buffer 
                              => Digit in '0' .. '9' | 'A' .. 'F')));
      
      pragma Assert 
        ((if Buffer'Length > Max_Nibbles then
            (for all I in Buffer'First .. (Buffer'Last - Max_Nibbles)
               => Buffer(I) = '0')));
      
   end Encode;
   
   ----------------------------------------------------------------------------
   function  Encode (Value: Modular_Value; Use_Case: Set_Case := Lower_Case)
                    return String
   is 
      Buffer: String (1 .. Max_Nibbles);
      -- An intermediate buffer that can handle the largest possible value.
      -- We will trim all zeros from the most significant portion before
      -- returning
      
      Start: Positive := Buffer'First;
   begin
      Encode (Value    => Value,
              Buffer   => Buffer,
              Use_Case => Use_Case);
      
      pragma Assert (if Value = 0 then
                       (for all Digit of Buffer => Digit = '0')
                     else
                       (for some Digit of Buffer => Digit /= '0'));
      
      for I in Buffer'Range loop
         if Buffer(I) /= '0' then
            Start := I;
            exit;
         end if;
      end loop;
      
      return Buffer(Start .. Buffer'Last);
      
   end Encode;
end Hex.Modular_Codec;
