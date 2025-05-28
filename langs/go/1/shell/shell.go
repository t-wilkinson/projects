package shell

import (
	"bytes"
	"os/exec"
)

func RunShell(command string) (err error, out string, errout string) {
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	cmd := exec.Command("bash", "-c", command)
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err = cmd.Run()
	return err, stdout.String(), stderr.String()
}
