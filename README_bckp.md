markdown
# Battleship Fortran

A classic Battleship game implementation written in Fortran. This terminal-based game allows you to place your fleet of ships on a 10×10 grid and engage in strategic combat against an intelligent computer opponent. The computer AI uses a three-stage targeting strategy: beginning with random shots, then systematically hunting ships once a hit is detected, and finally following a directional pattern once it identifies a ship's orientation.

## Key Features

- Complete Battleship gameplay with 5 different ships (Carrier, Battleship, Destroyer, Submarine, Patrol Boat)
- Interactive ship placement with collision detection and boundary validation
- Option for random ship placement
- Sophisticated AI opponent with strategic targeting algorithms
- Dual-grid display showing both defense (your ships) and offense (your targeting) boards
- ASCII art logo and styled game interface
- Scoreboard tracking hits, misses, and sunken ships
- Demo mode to watch AI vs AI battles
- Configurable random seed for reproducible gameplay

# Layout and Architecture
```
└── darius/
    ├── c3-dariuszucker-darius-470889e/  # Updated version of the project
    │   ├── CMakeLists.txt               # Build configuration
    │   ├── LICENSE
    │   ├── README.md                    # Latest documentation
    │   ├── README.md_v1                 # Previous documentation versions
    │   ├── README.md_v2
    │   ├── battleships.f90              # Core game engine module
    │   ├── fpm.toml                     # Fortran Package Manager config
    │   ├── main.f90                     # Program entry point
    │   ├── misc.f90                     # Utility functions
    │   └── sleep.c                      # C implementation of sleep function
    │
    └── c3-dariuszucker-darius-ee4ee60/  # Earlier version of the project
        ├── CMakeLists.txt
        ├── LICENSE
        ├── README.md
        ├── battleships.f90
        ├── fpm.toml
        ├── main.f90
        ├── misc.f90
        └── sleep.c
```

```mermaid
mermaid
graph TD
    subgraph Battleships
        A[main.f90] --> B[battleships_mod]
        B --> C{Game Mode}
        C -->|Human vs AI| D[Interactive Game]
        C -->|AI vs AI| E[Auto Battle]
        A --> F[misc_mod]

        subgraph "Entry Point"
            A
        end

        subgraph "Game Engine"
            B
        end

        subgraph "Game Modes"
            D
            E
        end

        subgraph "Utilities"
            F
        end
    end

    A -.-> battleships_game["battleships_game (Program)"]
    B -.-> ships["ship (Type)"]
    B -.-> ai["ai() (AI Logic)"]
    B -.-> grid["grid_1(), grid_2() (Display)"]
    B -.-> ship_ops["setship(), clearship()"]
    B -.-> validate["checkship_grid(), checkship_collision()"]
    F -.-> string_utils["toUpper(), strip_spaces()"]
    F -.-> converters["convert_char2num(), convert_num2char()"]
    F -.-> sort["sort_1d()"]
    F -.-> system["sleep()"]
```

# Usage Examples

## Building the Game

### Using CMake

```bash
# Create build directory and configure project
cmake -B build

# Build the project
cmake --build build

# Run the game
./build/battleships
```

### Using Fortran Package Manager (FPM)

```bash
# Build and run with FPM
fpm run
```

## Game Basics

### Starting the Game

After building and running, you'll see the main menu with these options:
```
1. Place your ships
2. Begin battle!
3. Watch an automated battle
4. About
```

To navigate the menu, input the corresponding number (1-4).

### Ship Placement

```bash
# Select option 1 from main menu to place ships
# Select ships 2-6 to place them individually
# For each ship, enter bow coordinates (e.g., "A1") and direction (N, E, S, W)
```

For automated ship placement:
```bash
# Select option 7 from the ship placement menu
# Confirm with "Y" to place ships randomly
```

### Playing the Game

```bash
# After placing ships, return to main menu and select option 2
# Enter coordinates to target enemy ships (e.g., "A1", "J10")
# Game alternates between player and AI turns
```

### Watching AI vs AI Battle

```bash
# Select option 3 from the main menu
# Confirm with "Y" to watch automated battle
```

## Using Core Functions

### Grid Display Functions

Display a single grid:
```fortran
use battleships_mod, only: grid_1, CB
character(len=2), dimension(1:10, 1:10) :: my_grid = CB//CB

! Display defense grid
call grid_1(my_grid, 'd')
```

Display two grids side by side:
```fortran
use battleships_mod, only: grid_2, CB
character(len=2), dimension(1:10, 1:10) :: defense_grid = CB//CB
character(len=1), dimension(1:10, 1:10) :: offense_grid = CB

! Display both grids
call grid_2(defense_grid, offense_grid)
```

### Ship Management

Place a ship on the grid:
```fortran
use battleships_mod, only: ship, setship, CB
type(ship) :: my_ship
character(len=2), dimension(1:10, 1:10) :: grid = CB//CB

! Set up ship properties
my_ship%x = 1            ! Column A
my_ship%y = 1            ! Row 1
my_ship%z = 'N'          ! Direction North
my_ship%l = 3            ! Length 3
my_ship%nu = 'C'         ! Upper case symbol
my_ship%nl = 'c'         ! Lower case symbol
my_ship%s = CB           ! Status (not sunk)

! Place ship on grid
call setship(my_ship, grid)
```

Remove a ship from the grid:
```fortran
use battleships_mod, only: ship, clearship
! Using same ship and grid as above example

! Remove ship from grid
call clearship(my_ship, grid)
```

Randomly place all ships:
```fortran
use battleships_mod, only: ship, ranship, CB
type(ship), dimension(1:5) :: fleet
character(len=2), dimension(1:10, 1:10) :: grid = CB//CB

! Set up fleet properties (ships lengths, symbols, etc.)
! ...

! Place ships randomly
call ranship(fleet, grid)
```

### Scoreboard Display

Create and display a scoreboard:
```fortran
use battleships_mod, only: scoreboard, scoreboard_entity
type(scoreboard_entity), dimension(1:2) :: scores
character(len=32) :: title = "Score"

! Set up scores
scores(1)%n = "Player 1"  ! Name
scores(1)%s = 3           ! Current score
scores(1)%t = 5           ! Total possible score

scores(2)%n = "Player 2"
scores(2)%s = 2
scores(2)%t = 5

! Display scoreboard
call scoreboard(title, scores)
```

### String Manipulation Functions

Convert between character coordinates and numeric positions:
```fortran
use misc_mod, only: convert_char2num, convert_num2char
character :: col = 'A'
integer :: x

! Convert character to number
call convert_char2num(col, x)  ! x will be 1

! Convert number to character
call convert_num2char(5, col)  ! col will be 'E'
```

Convert a string with numeric value to an integer:
```fortran
use misc_mod, only: convert_charnum2num
character(len=2) :: text = "7 "
integer :: num

! Convert character number to integer
call convert_charnum2num(text, num)  ! num will be 7
```

Convert lowercase to uppercase:
```fortran
use misc_mod, only: toUpper
character :: c = 'd'

! Convert to uppercase
c = toUpper(c)  ! c will be 'D'
```

Remove spaces from a string:
```fortran
use misc_mod, only: strip_spaces
character(len=20) :: text = "Hello  World"

! Remove spaces
call strip_spaces(text)  ! text becomes "HelloWorld"
```

### Timing Control

Add a delay in milliseconds:
```fortran
use misc_mod, only: sleep

! Pause for 1 second
call sleep(1000)
```

# Battleships Fortran Game Implementation Deep Dive

The Battleship Fortran game is a classic naval strategy game implementation with both single-player and AI features. Let's explore three key features that form the core of this game's functionality.

## Key Feature 1: AI Targeting System

One of the most sophisticated aspects of the codebase is the AI opponent's targeting system. Far from being random, the AI employs a multi-stage strategy to efficiently hunt for and destroy the player's ships.

### Implementation Details

The AI targeting is implemented in the `ai` subroutine within the `battleships_mod` module, which employs a three-stage approach:

1. **Random Targeting (Stage 1)**:
   - Initially, the AI fires randomly until it lands a hit
   - Coordinates are generated using `RANDOM_NUMBER` and scaled to the 10x10 grid
   - Prevents firing at the same location twice

```fortran
CASE (1) ! New Target
  DO
    CALL RANDOM_NUMBER(random); random = random*10.0 + 1.0
    p0t%x = INT(random(1)); p0t%y = INT(random(2))
    p0t%s = defence(p0t%x,p0t%y)
    IF(p0t%s(2:2) == CB) EXIT ! not already fired at this position
  END DO
```

2. **Adjacent Space Scanning (Stage 2)**:
   - When a hit occurs, creates an array of adjacent positions (north, east, south, west)
   - Tests these positions systematically until finding a second hit
   - Uses a clever rotation mechanism to explore all valid adjacent cells

```fortran
ai_data%adjacent(1,1) = ai_data%centre(1)     ; ai_data%adjacent(1,2) = ai_data%centre(2) - 1 ! north
ai_data%adjacent(2,1) = ai_data%centre(1) + 1 ; ai_data%adjacent(2,2) = ai_data%centre(2)     ! east
ai_data%adjacent(3,1) = ai_data%centre(1)     ; ai_data%adjacent(3,2) = ai_data%centre(2) + 1 ! south
ai_data%adjacent(4,1) = ai_data%centre(1) - 1 ; ai_data%adjacent(4,2) = ai_data%centre(2)     ! west
```

3. **Linear Targeting (Stage 3)**:
   - After a second hit, determines the likely ship orientation
   - Creates a linear array of positions to target in that direction
   - Continues until the ship is sunk or all valid positions are exhausted
   - Includes logic to handle "about-face" when reaching grid boundaries

```fortran
ai_data%line(0,1:2) = ai_data%centre
! ... (positioning logic based on direction)
ai_data%line_pos = ai_data%clock*i
IF(ai_data%line_pos > 4 .OR. ai_data%line_pos < -4) THEN ! about-face at end of range
  ai_data%clock = -1*ai_data%clock; i = 1; ai_data%line_pos = ai_data%clock*i
END IF
```

The AI state is cleverly maintained between turns using the `ai_saved_data` structure, which stores the current targeting stage, coordinates of hits, directional information, and possible target positions. This gives the AI significant tactical intelligence when hunting ships.

## Key Feature 2: Ship Placement and Validation System

Ship placement forms the foundational gameplay element, with sophisticated validation to ensure ships are placed legally and don't overlap.

### Implementation Details

This feature spans multiple subroutines in `battleships_mod`:

1. **Data Structure**:
   - Uses the `ship` derived type to store ship attributes:
   ```fortran
   TYPE ship
     INTEGER :: x, y, l, h  ! position, length, hits
     CHARACTER (LEN=1) :: z, nu, nl, s  ! direction, symbols, state
     CHARACTER (LEN=16) :: n  ! name
   END TYPE ship
   ```

2. **Manual Ship Placement**:
   - The `shipdata` subroutine handles user input for ship positioning
   - Takes coordinates (e.g., "A1"), direction (N,E,S,W) and validates positioning

3. **Ship Position Validation**:
   - Two critical validation checks occur in separate routines:
   
   `checkship_grid`: Ensures ships stay within grid boundaries
   ```fortran
   SUBROUTINE checkship_grid(boat, c)
     ! Check if ship extends beyond grid when positioned
     SELECT CASE (boat%z)
       CASE ('N')
         DO i = 1, boat%l - 1
           IF(boat%y + i > 10) THEN
             c = c + 1  ! Error flag
             EXIT
           END IF
         END DO
       ! ... other directions
     END SELECT
   END SUBROUTINE
   ```
   
   `checkship_collision`: Prevents ships from overlapping
   ```fortran
   SUBROUTINE checkship_collision(boat, array, c)
     ! Check if ships would overlap at any point
     DO i = 0, boat%l - 1
       SELECT CASE (boat%z)
         CASE ('N')
           IF(LLT(CB, array(boat%x, boat%y + i)(1:1))) c = c + 1
         ! ... other directions
       END SELECT
     END DO
   END SUBROUTINE
   ```

4. **Random Ship Placement**:
   - The `ranship` subroutine generates random positions for ships
   - Places ships in order of descending length (largest ships first)
   - Uses looping validation until a valid position is found for each ship
   ```fortran
   CALL sort_1d(lengths, 'd')  ! Sort ship lengths in descending order
   DO i=1,SIZE(lengths)
     DO  ! Keep trying until valid position found
       ! Generate random position and orientation
       CALL RANDOM_NUMBER(ran)
       ! ... position and validation code
       IF(c1 == 0 .AND. c2 == 0) EXIT  ! Valid position found
     END DO
   END DO
   ```

This system ensures the game board is properly set up with valid ship positions, whether the player places ships manually or the computer generates them randomly.

## Key Feature 3: Game Board Representation and Rendering

The game's visual representation is critical for player experience, providing clear feedback on game state.

### Implementation Details

1. **Board Representation**:
   - Uses 2D character arrays to store grid state:
   ```fortran
   CHARACTER (LEN=2), DIMENSION(1:10,1:10) :: defence, comp, offence*1
   ```
   - First character represents ship identity ('A', 'B', 'C', 'D', 'S')
   - Second character represents hit status (space for untouched, '*' for hit, '.' for miss)

2. **Grid Display**:
   - Multiple display routines handle different views:
   
   `grid_1`: Shows a single grid with coordinate labels
   ```fortran
   SUBROUTINE grid_1(a1, a)
     ! Display a single grid with coordinates and optional compass
     ! Used for setup and computer grid display
   ```
   
   `grid_2`: Shows side-by-side grids for player defense and offense
   ```fortran
   SUBROUTINE grid_2(a1, a2)
     ! Display defense and offense grids side by side
     ! Complete with coordinate labels and compass direction
   ```

3. **Ship Status Visualization**:
   - The `setship` and `clearship` routines handle rendering ships on the grid
   - Sets appropriate character codes for bow and body of ships
   ```fortran
   SUBROUTINE setship(boat, array)
     array(boat%x, boat%y) = boat%nu // boat%s  ! Place bow
     ! Place body based on direction
     SELECT CASE (boat%z)
       CASE ('N')
         DO i=1,boat%l-1
           array(boat%x,boat%y+i) = boat%nl // boat%s
         END DO
       ! ... other directions
     END SELECT
   END SUBROUTINE
   ```

4. **Scoreboard Display**:
   - The `scoreboard` subroutine renders game statistics:
   ```fortran
   SUBROUTINE scoreboard(title, entities)
     ! Display title and score information
     DO i=1,SIZE(entities)
       WRITE(stdout,"(4x,2a,i0,a,i0)") TRIM(entities(i)%n), ' : ', entities(i)%s, ' / ', entities(i)%t
     END DO
   END SUBROUTINE
   ```

The display system combines these elements to create a comprehensive user interface that effectively communicates the game state to players through ASCII graphics.

## Potential Improvement Areas

For developers looking to enhance these features:

1. **AI Targeting System**:
   - Add difficulty levels with different targeting strategies
   - Implement probabilistic targeting for "medium" difficulty
   - Add "personality" traits to make AI behavior less predictable

2. **Ship Placement System**:
   - Add drag-and-drop functionality for modern interfaces
   - Implement validation visualizations (highlight invalid positions)
   - Add preset formations or suggested placements

3. **Board Representation**:
   - Create alternative display modes (e.g., graphical rendering)
   - Add animation for hits, misses, and sinking ships
   - Implement color coding for better visibility

This Battleships implementation demonstrates a well-structured approach to game design in Fortran, with modular components that handle specific game functions while maintaining clear separation of concerns.

# Implemented User Storys

## Core Gameplay
- [ ] As a player, I want to play a game of Battleship against an AI opponent, so that I can enjoy a classic naval strategy game.
- [ ] As a player, I want to place ships on my grid manually, so that I can use strategic positioning.
- [ ] As a player, I want to have my ships automatically placed on the grid, so that I can start playing quickly.
- [ ] As a player, I want to target specific coordinates on the opponent's grid, so that I can try to hit their ships.
- [ ] As a player, I want to see when I've hit or missed an opponent's ship, so that I can track my progress.
- [ ] As a player, I want to know when I've sunk an opponent's ship, so that I can update my strategy.
- [ ] As a player, I want to know when I've won or lost the game, so that I can have closure on the gameplay session.
- [ ] As a player, I want to view the game state on a clearly displayed grid, so that I can make informed targeting decisions.
- [ ] As a player, I want to view my health and score during gameplay, so that I can track how well I'm doing.

## Ship Management
- [ ] As a player, I want to place different types of ships (Destroyer, Cruiser, Submarine, Battleship, Aircraft Carrier) with varying lengths, so that I can follow traditional Battleship rules.
- [ ] As a player, I want to specify the orientation (North, East, South, West) of my ships, so that I can customize their placement.
- [ ] As a player, I want to see my ship placement on the defense grid, so that I can confirm their positions.
- [ ] As a player, I want to see which parts of my ships have been hit, so that I can track damage.

## AI Opponent
- [ ] As a player, I want to play against an AI opponent with intelligent targeting behavior, so that the game is challenging.
- [ ] As a player, I want the AI to use progressive targeting strategies when it hits my ships, so that gameplay feels realistic.

## Game Configuration
- [ ] As a player, I want to adjust delay settings during gameplay, so that I can control the pace of the game.
- [ ] As a player, I want to save and load configuration settings, so that my preferences persist between game sessions.

## Special Game Modes
- [ ] As a player, I want to watch an automated battle between two AI opponents, so that I can study strategies or enjoy a demonstration.
- [ ] As a spectator, I want to see clear visual differentiation between hits and misses, so I can follow the gameplay easily.

## UI/UX
- [ ] As a player, I want to see a stylized Battleship logo, so that the game feels thematic and engaging.
- [ ] As a player, I want to navigate through an intuitive menu system, so that I can access different game functions easily.
- [ ] As a player, I want to receive clear feedback on my actions and game events, so that I understand what's happening.
- [ ] As a player, I want to view information about the game, including credits and references, so that I can learn about its development.

## Technical
- [ ] As a user, I want the game to run smoothly on various operating systems, so that I can play regardless of my platform.
- [ ] As a user, I want proper error handling for invalid inputs, so that the game doesn't crash unexpectedly.
- [ ] As a developer, I want to use a random number generator with seed controls, so that I can create reproducible game states for testing.