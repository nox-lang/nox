package codegen

import (
	"fmt"
	"strings"

	"nox/internal/ast"
)

// emitAsyncFunc generates the pieces needed to make an `async func` (or an
// async instance/static method, when thisType is non-nil) callable and
// awaitable, on top of the shared nox_task runtime (lib/nox_thread.c):
//
