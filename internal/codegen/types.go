// Package codegen turns a Nox AST into a single C translation unit that is
// then compiled by tcc.
package codegen

import (
	"fmt"
	"strings"
)

