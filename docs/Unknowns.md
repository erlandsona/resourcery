# Unkowns

Are bugs/features part of the scope of this app?
OR, should we keep to the Epic level and integrate with tools like Jira
  to aggregate that data for the answers we're looking for?

Integration is inevitable but let's postpone it as long as possible.


Chris:
    - Is this project planning?
    - What's the mission for the scope?
    - How do I see the interaction with clients?
        - It's going to be challenging regardless
            I'll be interested to see how change
            requests will be handled...

        - New person starts a task they can give 13 point capacity
            but in three weeks they'll be on another project with 3 point capacity.
                - They'll handle themselves.

        - New person starts a task they can give 13 point capacity
            but in three weeks they'll be on another project with 3 point capacity.
                - Points average over time regardless of what you're working on.

NOTE:
```
module User ...
type alias UserOptions = { ...}
type alias UserId = String -- UUID
-- Should be `type UserId = UserId String`
type User = User { id: UserId, ...}
type UserTable = UserTable { nextId: Int,  users: Dict User}

addUser : UserOptions -> UserTable -> (UserId, UserTable)

getUser: UserTable -> UserId -> User

{uuid: UserInfo} -- Model
(UserId) -- Set String / UUID's

```
