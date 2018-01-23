Note: This fork is mainly for my solutions. Please see the upstream repo if you want to use this to learn Prolog.

Note: still very much in progress

prolog-koans
============

Geared towards learning prolog based on the 99 problems created by Werner Hett (https://sites.google.com/site/prologsite/) with permission.


Usage
============

Non-interactive run with swipl:
- In main directory, run 'swipl -sq runner.pl -g "go." -t halt.'

Interactive run in interpreter:
- Start your prolog interpreter in the main directory (I personally used swi-prolog and can verify this suite works with it).
- Consult the runner file with "[runner]."
- Run the test suite with "go."
- As the test suite goes, it prints hints for each test and stops on the first failure.
