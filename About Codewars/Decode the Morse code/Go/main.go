package kata

import (
	"strings"
)

func DecodeMorse(morseCode string) string {
	var res []string
	for _,x := range strings.Split(morseCode,"   ") {
		var r []string
		for _,y := range strings.Split(x," ") {
			r = append(r, MORSE_CODE[y])
		}
		res = append(res, strings.Join(r, ""))
	}
	return strings.Trim(strings.Join(res," "), " ")
}
