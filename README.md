# EXIT89

**EXIT89** is an evacuation model developed to simulate the evacuation of large populations from high-rise buildings, initially designed by the late [Dr. Rita F. Fahy](https://en.wikipedia.org/wiki/Rita_Fahy). The model was first published in her work, [*EXIT89: An Evacuation Model for High-Rise Buildings*](http://www.iafss.org/publications/fss/3/815).

This repository preserves and archives Dr. Fahy's original code, implemented in Fortran, as it was when received by [Dr. Enrico Ronchi](https://orcid.org/0000-0002-2789-6359). While the code remains untested in its current form, our objective is to maintain it in its original state, ensuring Dr. Fahy's contributions to evacuation modeling are accessible for future research and preserved as part of her legacy.

**Note:** The code is currently provided in its original form. While no updates have been made, improvements or adaptations may be considered in the future.

---

## Installation and Sample Runs

### 1. Hardware Requirements
EXIT89 requires an IBM-compatible PC with a 386 or higher CPU and at least 4 MB of physical memory.

### 2. Compiling the Code

```bash
gfortran -o Create_EXIT8908 Create_EXIT89_Data_File.f90 Read_Comma_Delimited_File.f90 Length.f90
```

This will create an executable file named `Create_EXIT8908`.

### 2. Loading the Software
To install EXIT89, copy all files into a single directory. The executable file for the model is named `EXIT8908.EXE`. See directory `data`.

### 3. Example Data Sets
Included in this repository are example data files for sample scenarios. For instance:
- **Example 1:** Nighttime Hotel Scenario with Disabled Occupants  
  **Input file:** `UOFUNGT3.DAT`

> [!TIP]
> To generate this file, run `Create_EXIT8908`, which is produced by compiling `Create_EXIT89_Data_File.f90`. 
> This program reads the CSV file located in `data` and creates the required DAT file for executing `EXIT89`.

### 4. Running EXIT89 (MS-Windows only)
To run the model, follow these steps:
1. Copy any chosen data file (e.g., `UOFUNGT3.DAT`) into a file named `DPHILTMP.DAT`.
2. Run the model with this dataset by typing `EXIT8908` at the command prompt.
3. The output will be saved in a file named `DPHILTMP.OUT`.

If you wish to save the output for future reference, rename `DPHILTMP.OUT` to correspond with the original input file name.

> [!NOTE] 
> The data directory contains both the executable and the DPHILTMP.DAT file. 
> To generate the result file, open a terminal, navigate to the data directory with `cd data`, and run `EXIT8908`.
