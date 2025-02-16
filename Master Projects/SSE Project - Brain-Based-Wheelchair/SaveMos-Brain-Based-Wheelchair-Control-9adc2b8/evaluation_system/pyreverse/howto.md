Use the `--ignore` option to exclude the `unit_test` directory when running `pyreverse`:

```bash
pyreverse evaluation_system --ignore unit_test --output png --project evaluation_system
```
The above command should be run from the root directory of the project.

### Explanation
1. **`evaluation_system`:** Specifies the root module or package for analysis.
2. **`--ignore unit_test`:** Excludes the `unit_test` directory and its contents.
3. **`--output png`:** Generates a `.png` file for the diagram using Graphviz.
4. **`--project evaluation_system`:** Sets the project name, which will be used in the output filenames.

---

### Expected Outputs
This command will generate two PNG files in the current working directory:
1. `classes_evaluation_system.png`: The UML class diagram.
2. `packages_evaluation_system.png`: The UML package diagram.

