namespace Enigma

type AlphabetMapping = string
type RingSetting = RingSetting of char
type WheelPosition = WheelPosition of char
type KnockOn = WheelPosition list
type PlugBoard = PlugBoard of Map<char, char>
type Rotor =
    { Mapping : AlphabetMapping
      KnockOns : KnockOn
      RingSetting : RingSetting }
type TranslationDirection = | Forward | Inverse
type Reflector = Reflector of AlphabetMapping
type Enigma = 
    { Left : Rotor * WheelPosition
      Middle : Rotor * WheelPosition
      Right : Rotor * WheelPosition
      Reflector : Reflector
      Plugboard : PlugBoard }