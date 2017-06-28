`erl_pp` doesn't support comments. But we *can* abuse them in like so:

```
% 73> erl_pp:expr({var,0,"\n% comment\n"}).
% [["\n% comment\n"]]
```

If comments were needed in the generated source, this is one way to do it. If they only needed to live on the top-level, could more easily just add a new syntax form.
