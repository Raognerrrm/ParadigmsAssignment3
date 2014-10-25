import os, random, re, subprocess, time, unittest
            
def program_result(filename, test_folder, program, program_folder, function, args=None):
    """
    Executes program.function on test_folder/filename and returns the result
    """

    # Change working directory
    os.chdir(program_folder)
    
    prog_process = subprocess.Popen(["ghci", program, "-v0"], stdin=subprocess.PIPE,
                                    stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                    universal_newlines=True)

    result, err = prog_process.communicate("{} \"../{}/{}\" {}".format(function, test_folder, filename, "" if args == None else str(args)))

    os.chdir("..")

    # If an exception was raised, get the message from the exception
    if len(err) > 0:
        result = err[len("*** Exception: "):]

    return result

def get_expected(filename, test_folder, extension):
    """
    Given a file, etc. factorial.code, returns in the corresponding expected output, e.g. from
    factorial.expected
    """
    
    if filename.endswith(extension):
        file_head = filename[:-len(extension)]

    infile = open("{}/{}.expected".format(test_folder, file_head))
    expected = infile.read()
    infile.close()

    return expected

def strip(string):
    """
    Strips all whitespace from a string
    """
    
    return re.sub("\s+", "", string)

class TestSemantics(unittest.TestCase):
    TEST_FOLDER = "Task 1 unit tests"
    PROGRAM_FOLDER = "compiler"
    PROGRAM = "Semantics.hs"
    FUNCTION = "parse_check"

    def program_result(self, filename):
        return program_result(filename, self.TEST_FOLDER, self.PROGRAM, self.PROGRAM_FOLDER, self.FUNCTION)

    def get_expected(self, filename):
        return get_expected(filename, self.TEST_FOLDER, ".code")

    def run_and_check(self, filename):
        self.assertEqual(strip(self.program_result(filename)), strip(self.get_expected(filename)))

    def test_empty(self):
        self.run_and_check("empty_file.code")

    def test_alphanum_ids(self):
        self.run_and_check("alphanum_id.code")
        self.run_and_check("bad_alphanum_id.code")

    def test_case_sensitivity(self):
        self.run_and_check("case_sensitivity_1.code")

    def test_empty_vars(self):
        self.run_and_check("empty_vars.code")

    def test_redefined_function(self):
        self.run_and_check("redefined_function.code")

    def test_same_names(self):
        self.run_and_check("same_name_variables_1.code")
        self.run_and_check("same_name_variables_2.code")
        self.run_and_check("same_name_variables_3.code")
        self.run_and_check("same_name_variable_function.code")

    def test_undefined_function(self):
        self.run_and_check("undefined_function.code")

    def test_undefined_variable(self):
        self.run_and_check("undefined_variable_1.code")

    def simplest_prog(self):
        self.run_and_check("simplest_prog.code")

    def test_factorial(self):
        self.run_and_check("factorial.code")

    def test_everything(self):
        self.run_and_check("everything.code")

class TestIntermediateProg(unittest.TestCase):
    TEST_FOLDER = "Task 3a unit tests"
    INTERPRETER_FOLDER = "interpreter"
    INTERPRETER = "Interpreter.hs"
    INTERPRET_FUNCTION = "printInterpreted"

    def interpret_result(self, filename, args=None):
        return program_result(filename, self.TEST_FOLDER, self.INTERPRETER, self.INTERPRETER_FOLDER,
                              self.INTERPRET_FUNCTION, args)

    def assertStrippedEqual(self, a, b):
        self.assertEqual(a.strip(), b.strip())

    def test_factorial(self):
        self.assertStrippedEqual(self.interpret_result("factorial.intermediate", [0]), "1")
        self.assertStrippedEqual(self.interpret_result("factorial.intermediate", [1]), "1")
        self.assertStrippedEqual(self.interpret_result("factorial.intermediate", [2]), "2")
        self.assertStrippedEqual(self.interpret_result("factorial.intermediate", [3]), "6")
        self.assertStrippedEqual(self.interpret_result("factorial.intermediate", [4]), "24")
        self.assertStrippedEqual(self.interpret_result("factorial.intermediate", [10]), "3628800")

    def test_divide_by_zero(self):
        self.assertStrippedEqual(self.interpret_result("divide_by_zero.intermediate", []).strip(), "Error: Division by zero")

    def test_greater_than(self):
        self.assertStrippedEqual(self.interpret_result("greater_than.intermediate", [1, 4]), "4")
        self.assertStrippedEqual(self.interpret_result("greater_than.intermediate", [4, 1]), "4")

    def test_less_than(self):
        self.assertStrippedEqual(self.interpret_result("less_than.intermediate", [1, 4]), "1")
        self.assertStrippedEqual(self.interpret_result("less_than.intermediate", [4, 1]), "1")

    def test_register_reloading(self):
        self.assertStrippedEqual(self.interpret_result("register_reloading.intermediate", [1, 2, 3]), "6")
        self.assertStrippedEqual(self.interpret_result("register_reloading.intermediate", [3, 2, 1]), "2")

    def test_power_funky(self):
        self.assertStrippedEqual(self.interpret_result("power_funky.intermediate", [7, 6]), str(7**6))

class TestFullProg(unittest.TestCase):
    TEST_FOLDER = "Task 3b unit tests"
    COMPILER_FOLDER = "compiler"
    COMPILER = "Compiler.hs"
    COMPILE_FUNCTION = "printCompiled"
    INTERPRETER_FOLDER = "interpreter"
    INTERPRETER = "Interpreter.hs"
    INTERPRET_FUNCTION = "printInterpreted"

    def compile_result(self, filename):
        return program_result(filename, self.TEST_FOLDER, self.COMPILER,
                              self.COMPILER_FOLDER, self.COMPILE_FUNCTION)

    def interpret_result(self, filename, args=None):
        return program_result(filename, self.TEST_FOLDER, self.INTERPRETER, self.INTERPRETER_FOLDER,
                              self.INTERPRET_FUNCTION, args)

    def run_program(self, filename, args=None):
        tmpfile = open("{}/tmp".format(self.TEST_FOLDER), "w")
        tmpfile.write(self.compile_result(filename))
        tmpfile.close()
        output = self.interpret_result("tmp", args)
        os.remove("{}/{}".format(self.TEST_FOLDER, "tmp"))
        return output

    def assertStrippedEqual(self, a, b):
        self.assertEqual(a.strip(), b.strip())

    def test_add(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("addition.code", [x, y]), str(x+y))

    def test_sub(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("subtraction.code", [x, y]), str(x-y))

    def test_mul(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("multiplication.code", [x, y]), str(x*y))

    def test_div(self):
        x = random.randint(1, 10000)
        y = random.randint(1, 10000)
        self.assertStrippedEqual(self.run_program("division.code", [x, y]), str(x//y))

    def test_lt(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("less_than.code", [x, y]), str(1 if x<y else 0))

    def test_gt(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("greater_than.code", [x, y]), str(1 if x>y else 0))

    def test_cmp(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("comparison.code", [x, y]), str(1 if x==y else 0))

    def test_function_call(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("function_call.code", [x, y]), str(x+y))

    def test_factorial(self):
        self.assertStrippedEqual(self.run_program("factorial.code", [0]), "1")
        self.assertStrippedEqual(self.run_program("factorial.code", [1]), "1")
        self.assertStrippedEqual(self.run_program("factorial.code", [2]), "2")
        self.assertStrippedEqual(self.run_program("factorial.code", [3]), "6")
        self.assertStrippedEqual(self.run_program("factorial.code", [4]), "24")
        self.assertStrippedEqual(self.run_program("factorial.code", [10]), "3628800")

    def test_function_names(self):
        x = random.randint(-10000, 10000)
        y = random.randint(-10000, 10000)
        self.assertStrippedEqual(self.run_program("function_names.code", [x, y]), str(((x+y)*y)-y))

    def test_collatz_cycle(self):
        """
        How many Collatz Conjecture operations it takes to reach 1
        Project Euler problem 14
        """
        self.assertStrippedEqual(self.run_program("collatz_cycle.code", [7]), "16")
        self.assertStrippedEqual(self.run_program("collatz_cycle.code", [837799]), "524")

    def test_nth_prime(self):
        """
        Find the nth prime by trial division up to sqrt(n)
        Project Euler problem 7
        """
        self.assertStrippedEqual(self.run_program("nth_prime.code", [1]), "2")
        self.assertStrippedEqual(self.run_program("nth_prime.code", [10]), "29")
        self.assertStrippedEqual(self.run_program("nth_prime.code", [100]), "541")

    @unittest.skip("Takes too long")
    def test_big_primes(self):
        self.assertStrippedEqual(self.run_program("nth_prime.code", [1000]), "7919")
        self.assertStrippedEqual(self.run_program("nth_prime.code", [10001]), "104743")

    def test_non_returning(self):
        self.assertStrippedEqual(self.run_program("non_returning.code", [5]),
                                 "Error: Instructions exhausted (possibly due to a non-returning function)")

    def test_unassigned_variable(self):
        self.assertStrippedEqual(self.run_program("unassigned_variable.code", [37]), "37") # VARS variables implicitly set to 0

    def test_nested_ifs(self):
        self.assertStrippedEqual(self.run_program("nested_ifs.code", [1, 1, 0]), "1")
        self.assertStrippedEqual(self.run_program("nested_ifs.code", [1, 0, 1]), "2")
        self.assertStrippedEqual(self.run_program("nested_ifs.code", [1, 0, 0]), "0")
        self.assertStrippedEqual(self.run_program("nested_ifs.code", [0, 1, 0]), "3")
        
if __name__ == "__main__":
    unittest.main(exit=False)
