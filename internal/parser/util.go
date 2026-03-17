package parser

import "strconv"

func parseIntLiteral(s string) int64 {
	v, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		// Fall back to unsigned parsing then reinterpret, for very large literals.
