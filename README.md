# ASAP-HEX
ANNEXI-STRAYLINE AURA Public (ASAP) Repository - HEX subsystem

This subsystem is a member of the larger [ASAP Repository](https://github.com/annexi-strayline/ASAP)

This subsystem provides formally verified generic hex digit interpretation facilities. Verification was completed with SPARK. It is designed for high-speed/low-latency processing of untrusted input data containing hexadecimal values that must be interpreted.

Since these are verified, so long as the preconditions are met (specific verification subprograms are provided), all runtime checks may be safely disabled. For very high latency applications, these facilities may be more performant than S'Value.
