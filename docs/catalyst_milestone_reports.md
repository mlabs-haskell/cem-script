# Milestone 3

## Summary

Changes:

* Running in emulated environment by CLB
* Rendering CEMScript state graphs

## State graph examples

![](./auction-state-graph.png)


Source code:

```
digraph Creator {
rankdir=LR;
node [shape="dot",fontsize=14,fixedsize=true,width=1.5];
edge [fontsize=11];"Void In" [color="orange"];"Void Out" [color="orange"];"Void In" -> NotStarted [label="Create (stage Open)"];
NotStarted -> CurrentBid [label="Start (stage Open)"];
CurrentBid -> CurrentBid [label="MakeBid (stage Open)"];
CurrentBid -> Winner [label="Close (stage Closed)"];
Winner -> "Void Out" [label="Buyout (stage Closed)"];
}
```
