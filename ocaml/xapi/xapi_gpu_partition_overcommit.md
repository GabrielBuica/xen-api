# The overcommit cost of letting a suspended VM keep its partition

CP-314165 (V-05) · `MIG-P3-25 §d` · for PM, before the release goes out.

## What changed

Suspending a VM used to release its share of the card. It no longer does, if
that share is a specific NVIDIA MIG partition. The VM keeps the partition
while it is away, and gets that same one back when it resumes.

## Why it had to

A share of a card is interchangeable with any other share of the same size. A
partition is not: it is a specific piece of specific silicon, and a VM bound
to one that resumes onto a different one has resumed onto different hardware.
The guest's driver state, and anything it has cached about the device, is
about the piece it had.

So the choice was between a VM that may fail to resume and a partition that
sits idle. This is the second.

## What it costs

**A partition held by a suspended VM is capacity nobody else can use.** It is
not reported as free, it will not be offered to a starting VM, and it stays
that way for as long as the VM stays suspended — which is indefinite, because
nothing obliges anyone to resume.

Concretely, on a card divided into seven partitions with three VMs suspended,
the host advertises four. An operator who suspends VMs to free capacity for
other work will find that it does not.

Two consequences worth stating plainly:

- **There is no timeout and no eviction.** A suspended VM holds its partition
  until it resumes or is destroyed. If a partition needs to be reclaimed, the
  VM holding it has to be shut down — and it will then not resume onto that
  partition, because it will not have one.
- **The refusal is visible and specific.** If an operator re-divides or
  re-places out of band so that a suspended VM's partition is taken by the
  time it resumes, the resume fails with `GPU_PARTITION_IN_USE`, naming the
  partition and the VM now occupying it — rather than a generic "host full".
  This is deliberate: freeing that one partition is the remedy, and a generic
  admission failure would not tell the operator which one.

## What it does not cost

Card-level accounting is unchanged. A VM with a whole-card or a classic vGPU
still releases it on suspend exactly as before. This divergence applies only
to partition-grain assignment, and the lifecycle table's
"exactly one divergence" check exists to stop a second one appearing
unnoticed.

## The decision PM is being asked to confirm

That indefinitely-held capacity is the right trade against resume failures.
If it is not, the alternative is to release the partition on suspend and
accept that a resuming VM may be refused — which is the behaviour this
change replaces, and which is worse in a way the operator cannot plan around.

Scope: `FR-008` (VM lifecycle support), `FR-009` (host admission control).
