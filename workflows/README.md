# Setting up sylc on Archer2

- Sylc submit jobs from home. This does not really work, because the output and error logs are lost. Slurm cannot write to `\home` because the filesystem is not  mounted on the nodes. Symlinking to `\work` does not help this case.

## Run the workflows

From this folder ( and only this folder )

```bash
source env.sh # Setup the environment
cylc install # Create directory structures for the jobs
cylc play # Run the jobs
```

