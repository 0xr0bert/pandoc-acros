# pandoc-acros

This is a pandoc filter which replaces acronyms.

In the YAML metadata simply define the acronyms in the YAML header of the document:

```yaml
- short: dry
  long: don't repeat yourself
- short: ncd
  long: non-communicable disease
```

They can then be used like `[dry]{.ac}`.

The following options are available (capitalise the `a` to make the first letter of the long-form plural).

| Class | Description |
|-|---|
| `.ac` | First use---full; subsequent---short |
| `.acs` | Short |
| `.acl` | Long |
| `.acf` | Full |
| `.acp` | First use---full; subsequent---short; plural |
| `.acps` | Short plural |
| `.acpl` | Long plural |
| `.acpf` | Full plural |
| `.acstar` | First; don't record usage |
| `.acsstar` | Short; don't record usage |
| `.aclstar` | Long; don't record usage |
| `.acfstar` | Full; don't record usage |
| `.acpstar` | First; plural; don't record usage |
| `.acpsstar` | Short plural; don't record usage |
| `.acplstar` | Long plural; don't record usage |
| `.acpfstar` | Full plural; don't record usage