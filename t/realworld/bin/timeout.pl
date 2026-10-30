#!/usr/bin/perl
# usage: timeout.pl SECONDS cmd args...   exit 124 on timeout (SIGKILL).
use strict; use POSIX ":sys_wait_h";
my $secs = shift;
my $pid = fork();
die "fork: $!" unless defined $pid;
if ($pid == 0) { setpgrp(0, 0); exec @ARGV or die "exec: $!"; }
my $deadline = time + $secs;
while (1) {
  my $r = waitpid($pid, WNOHANG);
  if ($r == $pid) { exit($? & 127 ? 128 + ($? & 127) : $? >> 8); }
  if (time >= $deadline) { kill 'KILL', -$pid; kill 'KILL', $pid; waitpid($pid, 0); exit 124; }
  select(undef, undef, undef, 0.1);
}
