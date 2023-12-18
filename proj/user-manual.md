# User Manual for The Knight's Tour (10x10 Board)

## Explore The Knight's Tour with Lisp

Follow these steps to embark on The Knight's Tour adventure on a 10x10 board using your Lisp interpreter.

### 1. Open Lisp Interpreter:

   - Launch your Lisp interpreter in the terminal or command prompt. Here are some popular Lisp implementations to consider:

1. **SBCL (Steel Bank Common Lisp):**
    - A high-performance, open-source Common Lisp implementation known for its speed and active maintenance.

2. **Clozure CL (CCL):**
    - A fast, mature, and open-source Common Lisp implementation supporting native threads and providing good performance.

3. **GNU CLISP:**
    - A notable implementation with portability across various platforms, featuring an interactive environment (REPL) and a bytecode compiler.

4. **ECL (Embeddable Common Lisp):**
    - A lightweight, embeddable Common Lisp implementation designed for easy integration into other applications, supporting multiple backends.

5. **Allegro CL:**
    - A commercial Common Lisp implementation recognized for its speed and advanced features, including a native compiler and multi-threading support.

6. **LispWorks:**
    - A commercial implementation offering a comprehensive development environment, supporting various platforms, and providing advanced debugging and profiling tools.

7. **Chicken Scheme:**
    - While not Common Lisp, Chicken Scheme is a practical and lightweight Scheme implementation that compiles Scheme code to C, making it efficient for various applications.

8. **Racket:**
    - A multi-paradigm programming language including a Scheme interpreter, extensible and often used for teaching and research in programming languages.

9. **Clojure:**
    - A modern Lisp dialect running on the Java Virtual Machine (JVM), designed for concurrent and functional programming, emphasizing immutability.

    These are just a few examples; there are many other Lisp implementations and dialects. The choice depends on factors such as performance requirements, platform compatibility, and personal preferences.

### 2. Load and Compile Files:

   - Load and compile all the necessary files with the following command:
     ```lisp
     CL-USER> (load "~/Desktop/IA/proj/project.lisp")
     ```

### 3. Run the Program:

   - Execute the main program using the following command:
     ```lisp
     CL-USER> (main)
     ```

### 4. Choose a Problem:

   - Select any problem from the available options in the `boards.dat` file.

        ```dat
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
        (. . .)
        ```

### 5. Check the Result:

   - Review the solution in the `solutions.dat` file to witness the Knight's Tour path.

### 6. Monitor Execution Time:

   - Utilize the expressions in the `timers.lisp` file to assess the execution time of the program.

Enjoy your exploration of The Knight's Tour, and feel free to reach out for any assistance or inquiries!