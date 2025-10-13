# Interactivity in OptiTrust

This chapter explains the working of the OptiTrust scripts that, from a given
key keybinding, launch a task and open a browser to view the output.


## From keybinding to task

In VScode, keybindings are user-specific. They must be registered as described
in the instructions from `INSTALL.md`. For example, `F6` runs the task named
"view diff".

## Description of a task

The project file `.vscode/tasks.json` describes the tasks. Consider e.g. 
"view diff". This task executes a script `tools/view_results.sh`.
The argument provided to the script is "step_diff" to indicate what result 
we want to visualize, and the path of the current script as well as the cursor
line are passed to the script. 

```json
      {
        "label": "View diff",
        "type": "shell",
        "command": "tools/view_result.sh",
        "options": {
          "cwd": "${workspaceFolder}",
          "shell": {
            "args": ["-i"]
          }
        },
        "presentation": {
          "clear": true
        },
        "args": [
          "step_diff",
          "${relativeFile}",
          "${lineNumber}"
        ]
      },
```      

Note: the option "-i" is to allow launching GUI tasks, it might not be stricly
necessary if the "run_action" wrapper uses a auxiliary "watcher" process.

## Purpose and working of view_results

The script `tools/view_results.sh` performs the following action:

1. It compiles the OptiTrust library
2. It compiles the current user OptiTrust script
3. It executes the user script, passing the line number as argument, and the desired mode (e.g. "step_diff" or "full_trace").
4. If applicable, it launches the browser for viewing the result.

For the last part, the relevant scripts are listed in the section titled "Open the output" of `tools/view_results.sh`. They include `open_diff.sh` and `open_trace.sh`.

## Purpose and working of open_diff

The script `tools/open_diff.sh` computes the diff between `myscript_before.cpp` and `myscript_after.cpp`, assuming `myscript.ml` to be the user script.

The diff is encoded in base64, to be included inside an html page named `myscript_diff.html`, which is derived from `tools/web_view/diff_template.html` by filling specific holes such as the title of the page and the base64 encoding of the diff.

The html file is then opened using the script `tools/open_in_browser.sh`.

## Purpose and working of open_trace

The script `tools/open_trace.sh` is meant to open an interactive trace. 
If the mode "standalone-full-trace" is used, the trace is computed a standalone trace, following the same approach as for "open_diff".

However, the typical usage, which scales up better, is to produce a webpage using only the meta-data describing the steps in the trace. The rendering of each individual step is computed only on-demand, by means of a client-server interaction.

Concretely, a webpage is opened, and the webpage makes interactive requests to a webserver. This webserver loads a serialized version of the trace computed during the exection of the user script, and answers incoming queries from the user web interface.

The script `open_trace.sh` first compiles the server (to ensure that its binary is up to date), then executes the server. It waits until the server acknowledge being running.

Then, the script `open_trace.sh` opens the webpage at the URL: `http://localhost:6775/myscript_trace.html`, assuming `myscript.ml` to be the user script.

The implementation of the server is found in `tools/trace_server/trace_server.ml`.
The serialization of the trace is performed in the function `dump_full_trace_to_js`, 
when the flag `Flags.request_serialized_trace` is set, as is the case in mode "full_trace".

## Purpose and working of open_in_browser

The script `tools/open_in_browser.sh` takes as argument the path to an html page, and a windows title. It aims at opening the page in a given browser, by either launching a fresh browser, or by reusing an existing browser window in case one can be found. Reusing a window is much faster, and avoids the creation of numerous tabs.

The mechanism for reusing windows is based on "xdotools", which requires an X server ---Wayland won't work. To disable Wayland: `sudo sh -c 'echo "WaylandEnable=false" >> /etc/gdm3/custom.conf'`, then reboot.

The choice of the browser is, by default, "firefox" on Ubuntu, and "xdg-open" on other OS. If desired, add can customize the choice by adding to your `~/.bashrc`, e.g., `export OPTITRUST_BROWSER="chromium"`.

Due to VScode sandboxing, in most set-ups, the VScode tasks are generally unable to launch a GUI application. To work around the problem, we use the script `run_action.sh` described below to execute arbitrary commands outside the sandbox.


## Purpose and working of run_action

The `run_action.sh` script implements tooling to work around limitations of VScode, which executes tasks in a sandbox, hence is not able to properly execute features such as launching an external browser or running "xdotool" for giving the focus to a given window. This limitations are especially visible when VScode and/or the brower is installed using Snap, but even with a direct binary installation, VScode performs some form of sandboxing.

The script `run_action.sh` takes as argument a command line to execute, and writes this command line into a temporary file named `action.sh`. 

Independently, the OptiTrust user needs to execute a script named `watch.sh` that runs in the background. It is launched by means of the command `./watcher.sh`, which is just a shorthand for `./.vscode/watch.sh`. This script waits to observe modifications to the file `action.sh` (using the "inotify" tooling). When the script detects a change, it executes the command line found in `action.sh`. 

The output of that script is captured in a file named `action_out.txt`, whose contents is then reported as output of `action.sh`. This way, the user obtains the feedback of the requested command in the integrated terminal of VScode.

## Troubleshooting

To debug the script `open_in_browser.sh`, use the file `tools/test_shortcuts.sh`. Follow the instructions at the top of the file. Make sure to first launch `./watcher.sh` in a separate terminal before start.



