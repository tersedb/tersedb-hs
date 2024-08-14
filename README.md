TerseDB
=======

TerseDB is an independent _Entity Management System_ designed to be used alongside any existing
database system you may have running. It is a decoupled solution to the complex problem of
authorizing changes in a "multi-actor" environment; where many people (or systems) may be
manipulating, creating, and deleting many different things concurrently. TerseDB intends to provide
you with:

- the simplicity of only having to worry about storing and retreiving things, not the authorization
  of "who gets to view it"
- a simple, limited HTTP API for queries on entities
- plugins to various database vendors to allow for easier rich database queries, like full text
  search

## Semantics

### Identifiers

ID's are 12-byte random hexadecimal strings, similar to a MongoDB `ObjectId`. There are 5
variants of this concept - `ActorId`, `GroupId`, `VersionId`, `EntityId`, and `SpaceId`.

### Permission

There are two variants of permission - one for collections, and the other to describe a single
element of a collection. They are noted as `CollectionPermission` and `SinglePermission`,
respectively.

`CollectionPermission` can be one of the following: `Blind`, `Read`, `Create`,
`Update`, and `Delete`; where the lest amount of permission is `Blind` (can't see any elements),
and the most is `Delete` (can delete any element).

`SinglePermission` can be one of the following: `NonExistent`, `Exists`, `Adjust`, and `Obliterate`.
Conversely, the least amount of _granted_ permission is `Exists`, and the most is `Obliterate`
(similarly to `Read` and `Delete`), but `NonExistent` actually _restricts_ permission for the
actor. Also note that there is no `Create` analogue, as the single element of the collection
would already exist.

The idea is this permission system should be capable of building a heirarchy - a `SinglePermission`
can represent a _single_ collection - a `CollectionPermission` can bear many _single_ elements.
In this latter case, there is one additional dimension of permission - being exempt from
restrictions caused by single elements. In this case, we represent the exemption as either being
`Exempt` or `NotExempt`, in addition to its `CollectionPermission`.

### Actors

Actors can perform actions to the system - whether its to other actors, groups that actors
inhabit, entities being tracked, spaces of entities, or versions within those entities.
Any action performed on the system will be commited by __one or more__ actors.

The collection of actors are governed by those with recruiter rights.

### Groups

Groups grant permission to its actors, who are members of the group.

```mermaid
graph LR;
Actor1---Group1;
Actor2---Group1;
Actor3---Group1;
Actor2---Group2;
Actor3---Group2;
```

Membership is seen as a many-to-many relationship.

Groups can also inherit one another like a tree:

```mermaid
graph TD;
Group1-->Group2;
Group1-->Group3;
Group2-->Group4;
Group5-->Group6;
```

Permissions from each group are strictly additive - the effective permissions granted in
`Group4` will be the sum (or max, depending on how you want to look at it) of all permissions
granted individually in `Group1`, `Group2`, and `Group4`.

The collection of groups is governed by those with organizational rights.

The collections of memberships for groups are governed by those with membership rights for those
groups.

### Spaces, Entities, and Versions

Spaces are the collections of entities. Every entity belongs to exactly 1 space, which makes
spaces disjoint sets of entities:

```mermaid
graph TD;
Entity1-->Space1;
Entity2-->Space1;
Entity3-->Space1;
Entity4-->Space2;
Entity5-->Space2;
Entity6-->Space3;
Entity7-->Space3;
Entity8-->Space3;
```

Versions are linked lists, each cohesively belonging to a single entity. Every entity has at least
one version.

```mermaid
graph TD;
Version1-->Entity1;
Version2-->Entity1;
Version3-->Entity1;
Version4-->Entity1;
Version1-->Version2;
Version2-->Version3;
Version3-->Version4;
```

The collection of spaces is governed by those with universal rights.

The collections of entities and versions for spaces are governed by those with entity rights for
those spaces.

### References

Versions can reference other versions anywhere in the universe. The very existence of a reference
from `V1` to `V2` means that an actor with either `Create` or `Update` access to a version `V1`
also was able to see `V2` and decided to bind `V2` to `V1`. This can only be unbound by an
actor with the same permissions.

```mermaid
flowchart LR;
  Version1-- Reference -->Version2
  subgraph Space1
    subgraph Entity1
      Version1
    end
  end
  subgraph Space2
    subgraph Entity2
      Version2
    end
  end
```

The purpose of a reference is to capture a witnessed state in time.

### Subscriptions

Contrary to references, subscriptions bind to an entity rather than a specific version -
symbolizing the changing nature of things, and that we might want to subscribe to its latest
version.

```mermaid
flowchart LR;
  Version1-- Subscription -->Entity2
  subgraph Space1
    subgraph Entity1
      Version1
    end
  end
  subgraph Space2
    subgraph Entity2
      Version2
    end
  end
```

The purpose of a subscription is to capture a witnessed state, but stay up-to-date with
its changes over time.

### Scenarios

When starting an instance of TerseDB, you will always have an initial `adminActor` and a
`adminGroup`, which has the following rights:

- Recruiter `Delete` rights
- Organization `Delete` rights, with `Exempt`
- Universe `Delete` rights, with `Exempt`

Without this, nothing could be created!

#### Create an Actor and Group, and make it a Member

1. Have `adminActor` create an actor `A`
2. Have `adminActor` create a group `G`
3. Have `adminActor` grant Member `Create` rights to group `adminGroup` on behalf of group `G`
4. Have `adminActor` create a membership of `A` to `G`

#### Create a Space and let `A` create Entities within it

5. Have `adminActor` grant Universe `Read` rights to group `G`
6. Have `adminActor` create a space `S`
7. Have `adminActor` grant Entity `Create` rights to group `G` on behalf of space `S`

## Semantic Inconsistencies

There are a few issues to the design of this system, semantically speaking. It's not perfect,
but it suits the purposes it was designed for well enough.

- `Read` for a recruiter simply means that all Actors can be seen. There is no way to make
  specific accounts "private" or hidden.
- `Update` has no meaning for a few concepts within TerseDB:
  - Memberships
  We still permit the queries if you retain metadata between groups and its member actors.
