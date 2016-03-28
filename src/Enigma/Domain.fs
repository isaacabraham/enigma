namespace Enigma

type AlphabetMapping = char array
type RingSetting = RingSetting of char
type KnockOn = KnockOn of int
type PlugBoard = PlugBoard of Map<char, char>
type Rotor =
    { ID : int
      Mapping : AlphabetMapping
      KnockOns : KnockOn list }
type TranslationDirection = Forward | Inverse
type Reflector = Reflector of AlphabetMapping
type Enigma = 
    { Left : Rotor
      Middle : Rotor
      Right : Rotor
      Reflector : Reflector
      Plugboard : PlugBoard }