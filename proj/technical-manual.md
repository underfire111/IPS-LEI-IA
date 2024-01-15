# Technical Manual: The Knight's Tour (10x10 Board)

## Introduction

**Background:**

The Knight's Tour is a classic chess problem that involves moving a knight on a chessboard in such a way that it visits each square exactly once. In this technical manual, we present a comprehensive exploration of the problem on a 10x10 chessboard, employing various algorithms to find optimal solutions.

**Objectives:**

Our primary objective is to devise effective algorithms for solving the Knight's Tour problem efficiently. By implementing and analyzing multiple search strategies, we aim to uncover insights into their performance and trade-offs.

## Game Rules

### Board Configuration

- **Dimensions:** The game unfolds on a sprawling 10x10 board, offering a vast array of potential moves and strategic choices.

### Starting Point

- **Knight's Position:** The knight commences its journey from any square on the 1st row, providing players with flexibility in their starting moves.

### Removal Rules

1. **Double Values:**

   - When the knight lands on a square with a double value (e.g., 22), it gains the power to remove other squares with values that are multiples of 11.
   - Example: If the knight is placed on a square with the value 22, it can remove squares with the values 00, 11, 33, 44, 55, 66, 77, 88, and 99.

2. **Non-Double Values:**
   - When the knight occupies a square with a non-double value (e.g., 23), it becomes the harbinger of symmetry. It has the authority to remove the literal symmetric value on the board.
   - Example: If the knight is placed on a square with the value 23, the square with the value 32 shall be gracefully removed from the board.

### Scoring and Objectives

- **Scoring System:** The game revolves around accumulating scores based on the values of the squares the knight steps on.
- **Objective:** Players aim to reach a predefined score to achieve victory. However, beware of potential dead ends, as finding the optimal path is essential.

### Strategic Decision-Making

- **Optimal Paths:** Delve into the art of strategic planning, maximizing scores while navigating the intricate 10x10 landscape.
- **Risk and Reward:** Balance the pursuit of higher scores with the potential consequences of reaching a dead end, ensuring each move is a step toward victory.

## Knight's Move and Operators

### Knight's Move

- The knight moves in an "L" shape: two squares in one direction (horizontally or vertically) and then one square perpendicular to the initial direction.
- This unique move allows the knight to access various parts of the board quickly.

### Operators

- **Move Operator:** Execute the knight's move by specifying the destination square. For example, if the knight is on square (x, y), the move operator could be (x + 2, y + 1) or (x - 1, y - 2).
- **Score Operator:** Upon landing on a square, calculate the score based on its value and update the player's total score.

- **Removal Operator:** Implement removal rules based on the type of square the knight occupies. Use conditional statements to check for double values or non-double values and remove squares accordingly.

## Algorithms

### Breadth-First Search (BFS)

**Algorithm Overview:**

1. **Description:**

   - BFS is a graph traversal algorithm that explores all vertices at the current depth before moving on to the vertices at the next depth.
   - It uses a queue data structure to keep track of the order in which vertices are visited.

2. **Key Components:**
   - **Queue:** Stores vertices to be explored in the order they are discovered.
   - **Visited Set:** Keeps track of visited vertices to avoid revisiting them.

**Technical Details:**

1. **Data Structures:**

   - **Queue Implementation:** Typically implemented using a queue data structure or a simple list.
   - **Visited Set:** Implemented using a hash set or an array to efficiently check if a vertex has been visited.

2. **Complexity:**
   - **Time Complexity:** O(V + E), where V is the number of vertices and E is the number of edges.
   - **Space Complexity:** O(V), where V is the number of vertices.

### Depth-First Search (DFS)

**Algorithm Overview:**

1. **Description:**

   - DFS explores as far as possible along each branch before backtracking.
   - It can be implemented using recursion or an explicit stack.

2. **Key Components:**
   - **Stack/Recursion:** Used to keep track of vertices to visit.
   - **Visited Set:** Prevents revisiting vertices.

**Technical Details:**

1. **Data Structures:**

   - **Stack Implementation:** Can be implemented using a stack data structure or the call stack in the recursive version.
   - **Visited Set:** Similar to BFS, implemented using a hash set or an array.

2. **Complexity:**
   - **Time Complexity:** O(V + E), where V is the number of vertices and E is the number of edges.
   - **Space Complexity:** O(V), where V is the number of vertices (for recursive DFS, the call stack contributes to space complexity).

### A\* (A-star)

**Algorithm Overview:**

1. **Description:**

   - A\* is an informed search algorithm that uses heuristics to find the most optimal path.
   - It evaluates nodes based on a combination of the cost to reach the node and a heuristic estimate of the remaining cost.

2. **Key Components:**
   - **Open List:** Stores nodes to be evaluated.
   - **Closed List:** Keeps track of nodes already evaluated.
   - **Heuristic Function:** Estimates the cost from a node to the goal.

**Technical Details:**

1. **Data Structures:**

   - **Priority Queue (Min-Heap):** Used for the open list to efficiently retrieve the node with the lowest total cost.
   - **Visited Set (Closed List):** Ensures nodes are not re-evaluated.

2. **Heuristic Function:**

   - **Admissible Heuristic:** Underestimates the true cost to reach the goal.
   - **Consistent Heuristic:** Satisfies the triangle inequality.

3. **Complexity:**
   - **Time Complexity:** Depends on the heuristic but generally O(b^(h)) where b is the branching factor and h is the depth of the optimal path.
   - **Space Complexity:** O(b^h), where b is the branching factor and h is the maximum depth in the search space.

### Iterative Deepening A* (IDA*)

**Algorithm Overview:**

1. **Description:**

   - IDA* is a variation of A* that uses iterative deepening to find the optimal path without requiring excessive memory.
   - It repeatedly performs depth-limited searches with increasing depth thresholds.

2. **Key Components:**
   - **Depth Limit:** The maximum depth explored during each iteration.

**Technical Details:**

1. **Iteration Process:**

   - Perform A\* with increasing depth limits until the goal is found.
   - Memory consumption is reduced compared to traditional A\*.

2. **Complexity:**
   - **Time Complexity:** Similar to A\*, but with a larger constant factor due to repeated searches.
   - **Space Complexity:** O(bd), where b is the branching factor and d is the maximum depth.

### Simplified Memory-Bounded A* (SMA*)

**Algorithm Overview:**

1. **Description:**

   - SMA* is a memory-bounded variant of A* designed for environments with limited memory resources.
   - It sacrifices optimality to ensure feasibility within memory constraints.

2. **Key Components:**
   - **Memory Limit:** Restricts the number of nodes stored in memory.

**Technical Details:**

1. **Memory Management:**

   - Prunes the search tree to fit within the specified memory limit.
   - May use various strategies to decide which nodes to discard.

2. **Trade-Offs:**

   - Sacrifices optimality to maintain feasibility within memory constraints.
   - The choice of which nodes to discard affects the quality of the solution.

3. **Complexity:**
   - **Time Complexity:** Similar to A\*, but with increased dependence on memory management strategies.
   - **Space Complexity:** O(m), where m is the memory limit.

## Implementation

### Programming Language:

The implementation of " The Knight's Tour" was crafted using Common Lisp, a powerful and expressive programming language known for its flexibility and symbolic computation capabilities.

### Project Structure:

The project is organized into distinct files, each serving a specific purpose within the implementation. Here's an overview of the directory structure:

1.  **project.lisp:** This file serves as the central hub, responsible for loading other code files, managing file read and write operations, and handling user interactions. It acts as the orchestrator of the entire project.

2.  **puzzle.lisp:** The code within this file is intricately tied to the core problem-solving aspects of " The Knight's Tour." It encapsulates the logic and functionalities related to the knight's movement, square removal, and scoring mechanisms.

3.  **search.lisp:** This directory contains the heart of the algorithmic implementations, housing the following search algorithms:

    - **BFS (Breadth-First Search):**

      Responsible for exploring the game board in a breadth-first manner, providing a foundation for systematic exploration.

    - **DFS (Depth-First Search):**

      Implements the depth-first search algorithm, allowing for in-depth exploration of potential paths.

    - **A\* (A-star):**

      Incorporates the A\* algorithm, a heuristic-based search method that optimizes the exploration process for finding an optimal path.

    - **IDA\* (Iterative Deepening A\*):**

      If implemented, IDA\* is found here, showcasing an iterative deepening approach to achieve optimal solutions without excessive memory usage.

    - **RBFS (Recursive Best-First Search):**

      If implemented as a bonus, the logic for Recursive Best-First Search can be found within this directory.

    - **SMA\* (Simplified Memory-Bounded A\*):**

      If implemented, this section contains the algorithmic logic for SMA\*, emphasizing memory efficiency while navigating the game board.

4.  **boards.dat:** This file contains the boards proprosed to study with the following fomatation:

        Problem: A
        Objective: 70
        02 20 44 NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL 03 30 NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL 22 NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL
        -----------------------------

5.  **solution.dat:** This file is where the solution will be writen.

        ###############################
        ###############################
        ### {Algorithm}
        ### Goal: {--}
        ###############################

        Score: {--}
        -- Kn 44 -- -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --
        -- 03 30 -- -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --
        -- -- -- 22 -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --
        -- -- -- -- -- -- -- -- -- --

        (. . .)

        ###############################
        ### Final score: {--}
        ### Penetrance: {--}
        ### Branching factor: {--}
        ###############################
        ###############################

## Design Choices

### Global Variables:

Global variables, such as the board, positions-map, and score, are declared as global due to their immutability during program execution. They remain constant and do not change, making them accessible from any part of the program without the need for redundant parameter passing.

- **Board (10x10 matrix):** This matrix contains all the squares of the board and their values. Using a global variable streamlines access to the board and ensures its availability throughout the execution.
- **Positions-map (Hash-table):** This table maps square values to coordinates, facilitating the identification of doubles and symmetrics on the board.
- **Score (Goal):** An integer representing the desired score. This global variable aids in tracking the progress of a node towards the solution.

### Node Attributes:

Our node comprises four key attributes:

- **Hash-table:** This table captures the knight's current position and all squares already stepped on and removed (doubles or symmetrics). Storing the state as a list of movements rather than the entire board significantly reduces memory usage.
- **Current Score:** This attribute helps track the progress of a node towards the goal. It provides a measure of how close or far a solution is.
- **Parent Node:** Knowing the parent node allows for backtracking and understanding the path taken to reach the current state.
- **List with f, g, h:** These values aid in evaluating the node's cost (f), depth (g), and heuristic (h). These attributes play a crucial role in the A\* algorithm by guiding the search efficiently.

### Heuristics:

**Percentual Distance Heuristic:**
\[ h(x) = \left(1 - \frac{m(x)}{n}\right) \times 100 \]

- \( m(x) \): Current score.
- \( n \): Goal.

This heuristic assesses the percentage of distance covered towards the goal, providing a dynamic measure of progress.

**Movements Left Heuristic:**
\[ h(x) = \frac{o(x)}{m(x)} \]

- \( m(x) \): Average per square of the points on the board \( x \).
- \( o(x) \): Number of points remaining to reach the goal.

This heuristic estimates the number of movements left until the solution, integrating both the average per square and the remaining points.

**Average Progression Heuristic:**
\[ h(x) = -\frac{m(x)}{n(x)} \]

- \( m(x) \): Current score.
- \( n(x) \): Node depth.

This heuristic quantifies the average progression per movement, providing insights into the efficiency of the search algorithm.

## Code Explanation

**Game Operators:**

```lisp
(defun knight-start-position (x y)
  "Places the knight at the board."
  . . . )

(defun knight-move-to (current-state coordinates)
  "If possible, move the knight to the given location"
  . . . )

(defun knight-can-move-to (current-state)
  "Check for all possible moves."
  . . . )

(defun knight-can-move (current-state)
  "Check if the knight still have any move available."
  . . . )
```

**Nodes:**

```lisp
(defun create-node (state &optional parent)
  "Creates a new node."
  . . . )

(defun partenogenese (node &optional heuristic)
  "Creates the node successors."
  . . . )

(defun get-node-depth (node)
  "Returns the node depth."
  . . . )
```

**Heuristics:**

```lisp
(defun calc-percentual-distance (node)
  "Calculates the percentual distance."
  . . . )

(defun calc-average-progression (node)
  "Calculates the average progression per movement."
  . . . )

(defun calc-movements-left (node)
  "Calculates how many moves are needed to reach the goal."
  . . . )
```

**Metrics:**

```lisp
(defun penetrance (depth total-number-nodes)
  "Calculates the penetrance."
  . . . )

(defun bisection (depth number-of-nodes &optional (min 0) (max number-of-nodes) (tolerance 0.1))
  "Calculates the bisection of min and max."
  . . . )
```

**Algorithms:**

```lisp
(defun breadth-first-search ()
  "Performs Breadth-First Search on a graph."
  . . . )

(defun depth-first-search ()
  "Performs Depth-First Search on a graph."
  . . . )

(defun a* (heuristic)
  "Performs A* on a graph."
  . . . )

(defun ida* (heuristic)
  "Performs IDA* on a graph."
  . . . )

(defun sma* (heuristic &optional (limit 10))
  "Performs SMA* on a graph."
  . . . )
```

## Results and Evaluation

### Test Cases

The algorithm was tested on various game scenarios, represented by different problem instances labeled A, B, C, D, and E. Each problem instance reflects distinct challenges and characteristics, allowing for a comprehensive evaluation of the algorithm's performance.

### Metrics

**Penetrance:**

- Reflects the algorithm's effectiveness in penetrating through the search space, with values closer to 1 indicating higher success rates.

**Branching Factor:**

- Represents the average number of child nodes generated per parent node, offering insights into the algorithm's exploration efficiency.

### Results

#### Depth-First Search

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.428571  |       3.5        |   72   |   0.000000   |
|    B    |  0.714286  |       7.0        |   65   |   0.000000   |
|    C    |  0.428571  |       7.0        |  282   |   0.000001   |
|    D    |  0.295455  |       22.0       |  665   |   0.000033   |
|    E    |  0.000110  |      9117.0      |   --   |   0.033340   |

Depth-First Search, while achieving relatively high penetrance, tends to have a higher branching factor, resulting in a more extensive exploration but potentially overlooking optimal paths.

#### Breadth-First Search

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.375000  |       4.0        |   72   |   0.000000   |
|    B    |  0.173913  |       23.0       |   60   |   0.000000   |
|    C    |  0.162162  |       18.5       |  282   |   0.000333   |
|    D    |  0.045802  |      131.5       |  668   |   0.003340   |
|    E    |  0.000110  |      9117.0      |   --   |   0.076672   |

Breadth-First Search, with a lower branching factor, explores the search space more systematically, making it suitable for scenarios where an optimal solution is sought.

#### A\* (A-Star)

- **Heuristic 1**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.333333  |       4.5        |   72   |   0.000000   |
|    B    |  0.173913  |       23.0       |   60   |   0.000000   |
|    C    |  0.153846  |       19.5       |  282   |   0.000000   |
|    D    |  0.045113  |      133.0       |  661   |   0.003333   |
|    E    |  0.000110  |      9117.0      |   --   |   0.693360   |

- **Heuristic 2**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.428571  |       3.5        |   72   |   0.000000   |
|    B    |  0.200000  |       20.0       |   60   |   0.000000   |
|    C    |  0.166667  |       19.0       |  282   |   0.000000   |
|    D    |  0.255319  |       23.5       |  665   |   0.000000   |
|    E    |  0.000110  |      9117.0      |   --   |   0.036667   |

- **Heuristic 3**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.428571  |       3.5        |   72   |   0.000000   |
|    B    |  0.190476  |       21.0       |   60   |   0.000000   |
|    C    |  0.146341  |       20.5       |  282   |   0.000000   |
|    D    |  0.190476  |       31.5       |  665   |   0.000000   |
|    E    |  0.000110  |      9117.0      |   --   |   0.613345   |

A\* with various heuristics demonstrates improved penetrance compared to basic search algorithms, utilizing informed search strategies to efficiently navigate the search space.
#### Iterative Deepening A* (IDA*)

- **Heuristic 1**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.375000  |        4.0       |   72   |   0.000000   |
|    B    |  0.666667  |        7.5       |   60   |   0.000000   |
|    C    |  0.285714  |       10.5       |  282   |   0.000000   |
|    D    |  0.371428  |       17.5       |  665   |   0.000000   |
|    E    |  0.000110  |      9117.0      |   --   |   0.073332   |

- **Heuristic 2**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.375000  |        4.0       |   72   |   0.000000   |
|    B    |  0.186046  |       21.5       |   60   |   0.000000   |
|    C    |  0.153846  |       19.5       |  282   |   0.000000   |
|    D    |  0.282608  |       23.0       |  665   |   0.000000   |
|    E    |  0.000110  |      9117.0      |   --   |   0.030000   |

- **Heuristic 3**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: | :----------: |
|    A    |  0.375000  |        4.0       |   72   |   0.000000   |
|    B    |  0.186046  |       21.5       |   60   |   0.000000   |
|    C    |  0.153846  |       19.5       |  282   |   0.000000   |
|    D    |  0.282608  |       23.0       |  665   |   0.000000   |
|    E    |  0.000110  |      9117.0      |   --   |   0.029999   |

IDA* combines the benefits of A* with iterative deepening, yielding improved results in terms of penetrance while maintaining a manageable branching factor.

#### Simplified Memory-Bounded A* (SMA*)

- **Heuristic 1**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: |   :------:   |
|    A    |  0.333333  |       4.5        |   72   |   0.000000   |
|    B    |  0.173913  |       23.0       |   60   |   0.000000   |
|    C    |  0.162162  |       18.5       |  282   |   0.000000   |
|    D    |  0.157895  |       38.0       |  661   |   0.000000   |
|    E    |  0.007143  |     71645.0      |   --   |   0.033340   |

- **Heuristic 2**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: |   :------:   |
|    A    |  0.428571  |       3.5        |   72   |   0.000000   |
|    B    |  0.047619  |       5.3        |   --   |   0.000000   |
|    C    |  0.076923  |       5.1        |   --   |   0.000000   |
|    D    |  0.050000  |       19.9       |   --   |   0.000000   |
|    E    |  0.050000  |      1275.0      |   --   |   0.000000   |

- **Heuristic 3**

| Problem | Penetrance | Branching Factor | Points | Run Time (s) |
| :-----: | :--------: | :--------------: | :----: |   :------:   |
|    A    |  0.333333  |       4.5        |   72   |   0.000000   |
|    B    |  0.047619  |       5.3        |   --   |   0.000000   |
|    C    |  0.076923  |       5.1        |   --   |   0.000000   |
|    D    |  0.387097  |       15.5       |  665   |   0.000000   |
|    E    |  0.009009  |     227217.0     |   --   |   0.000000   |

SMA\*, incorporating simplified memory constraints, demonstrates competitive penetrance with varying branching factors depending on the heuristic used. It offers a balance between memory usage and algorithm effectiveness.

In conclusion, the choice of algorithm and heuristic depends on specific requirements and constraints. A* with heuristic 1 and IDA* seem promising for striking a balance between efficiency and optimality, while SMA\* provides an alternative for scenarios with stringent memory limitations. The penetrance and branching factor metrics offer valuable insights into the algorithm's success and exploration efficiency, aiding in the selection of the most suitable approach for a given problem.

