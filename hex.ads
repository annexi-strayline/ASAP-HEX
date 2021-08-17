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

package Hex
  with SPARK_Mode => On
is
   pragma Preelaborate (Hex);
   pragma Assertion_Policy (Ignore);
   -- This package is validated
   
   type Set_Case is (Lower_Case, Upper_Case);
   
   subtype Hex_Character is Character
     with Static_Predicate
       => Hex_Character in '0' .. '9' | 'A' .. 'F' | 'a' .. 'f';
   
   function  Valid_Hex_String (Candidate: String) return Boolean
     is (Candidate'Length >= 1 and then
           (for all Char of Candidate => Char in Hex_Character))
   with 
     Inline => True,
          Global => null;
   -- Returns True if and only if Candidate represents a valid hexadecimal
   -- string.
   --
   
   ------------------
   -- Identify_Hex --
   ------------------
   subtype Valid_Exterior is Character
     with Static_Predicate => 
       Valid_Exterior in 
         Character'Val (9)   .. Character'Val (13)  |  -- HT,LF,VT,FF,CR
         Character'Val (32)  .. Character'Val (47)  |  -- Punctuation
         Character'Val (58)  .. Character'Val (64)  |  -- Punctuation
         Character'Val (123) .. Character'Val (126);   -- More punctuation
   -- Characters that are valid immediately before and after a valid
   -- hexadecimal string during identification of that string
   
   function  Demarcation_Precedent_Check (Source: String; First: Positive)
                                         return Boolean
     is (First = Source'First
           or else 
           (First > Source'First and then
              (Source(First - 1) in Valid_Exterior
                 or else
                 (Source(First - 1) in 'X' | 'x'
                    and then (case Source(Source'First .. First)'Length is
                                 when 3 =>
                                    Source (First - 2) = '0',
                                 when 4 .. Positive'Last =>
                                   (Source (First - 2) = '0' and then
                                      Source (First - 3) in Valid_Exterior),
                                 when others =>
                                    False)))))
   
     with 
     Inline => True,
     Global => null,
     Pre    => First in Source'Range
               and then Source(First) in Hex_Character;
   -- True for one of the four following conditions, provided
   -- Source(First) is a Hex_Character
   -- 1. First = Source'First
   -- 2. (First - 1 .. First) is a Valid_Exterior & Hex_Character
   -- 3. (First - 2 .. First) is ("0x" | "0X") or
   -- 4. (First - 3) is Valid_Exterior & Condition 3

   
   function  Demarcation_Subsequent_Check (Source: String; Last: Natural)
                                          return Boolean
     is (Source'Length in 0 .. 1
           or else Last = Source'Last
           or else Source (Last + 1) in Valid_Exterior)
     with
     Inline => True,
     Global => null,
     Pre    => Source'Length = 0
               or else (Last in Source'Range
                        and then Source(Last) in Hex_Character);
   -- True if and only if:
   -- Source(Last) is a Hex_Character, and then
   -- Last = Source'Last, or else
   -- Source(Last + 1) = Valid_Exterior
   
   
   ----------------------------------------------------------------------------
   procedure Identify_Hex (Source: in     String;
                           First :    out Positive;
                           Last  :    out Natural)
     with 
     Global  => null,
     Depends => ((First, Last) => Source),
     Post    => (if Last >= First then
                    Valid_Hex_String (Source (First .. Last))
                    and then Demarcation_Precedent_Check (Source, First)
                    and then Demarcation_Subsequent_Check (Source, Last)
                 else
                 (if Source'Length > 0 then
                     First = Source'First and Last = First - 1
                  else
                     First = 1 and Last = 0));
   -- Identify_Hex is a verified helper function which identifies the first
   -- (if any) valid unbroken hexadecimal string in the Source string, which
   -- is demarcated by the range First .. Last of Source
   --
   -- If a valid hexadecimal string cannot be identified within Source,
   -- First is set to Source'First, and Last is set to Source'First - 1
   -- (a null range).
   --
   -- This procedure attempts to avoid misidentification of arbitrary 
   -- "hexadecimal digits" that would otherwise appear frequently in most
   -- alphanumeric text. This is done by only identifying string of hexadecimal
   -- digits that are surrounded by an explicitly defined set of
   -- "Valid_Exterior" characters, which include exclusively graphical
   -- characters with tabs, spaces, CR/LF/FF, and most basic punctuation
   --
   -- This procedure also recognizes hexadecimal strings prepended with the
   -- common "0x" indicator.
   
end Hex;
