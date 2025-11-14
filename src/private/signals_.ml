let ignore_signals_ () =
  try
    Thread.sigmask SIG_BLOCK
      [
        Sys.sigpipe;
        Sys.sigbus;
        Sys.sigterm;
        Sys.sigchld;
        Sys.sigalrm;
        Sys.sigint;
        Sys.sigusr1;
        Sys.sigusr2;
      ]
    |> ignore
  with _ -> ()
