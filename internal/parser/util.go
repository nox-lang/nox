package parser

import "strconv"

func parseIntLiteral(s string) int64 {
	v, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		// Fall back to unsigned parsing then reinterpret, for very large literals.
		u, uerr := strconv.ParseUint(s, 10, 64)
		if uerr == nil {
			return int64(u)
		}
		return 0
	}
	return v
}

func parseFloatLiteral(s string) float64 {
	v, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0
	}
	return v
}
