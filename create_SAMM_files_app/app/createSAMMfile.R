#create SAMMII file in .stu format- for readability it is currently saving a txt file

# Function will take in already created datasets for running MM. This will be outlined in a README.
# Participants ID will usually be a combine id and day for easy recognition post processing
# missing baseline (-10 or 0) are filled in with each other
# Although cleaning is expected before this function use, this function will ensure no data is missing or no impossible demographic values

# Simone will need to check the text string inputted for SAMM below. I have made comments at specific points to highlight

createSAMMfile <- function(inputfile, # Input file (will be drag and dropped)
                           outputdir, # this is fixed and not changable as will be set to a temp directory to enable downlaod on the app
                           npoints = 5, # number of timepoints of the OGTT (5 or 7 choise will be fixed timepoints, custom will allo your own input of 5/7 timepoints see info button)
                           fasted_timepoint = NULL, # identify if there was a fasted timepoint ie. time before 0
                           timepoints = NULL, # list of custom time points can be 5 or 7 values only depending on npoints
                           model = "glucose", # glucose or cpeptide MM
                           glucose_protocol = 1, # will need changing but for now specifies the glucose dose was a single dose of 75g (varies depending on weight if under threshold)
                           FSD = 0.02, # error term for glucose model - to add more info
                           GEN1 = 2000, # error term for cpeptide model - to add more info
                           GEN2 = 0.001,# error term for cpeptide model - to add more info
                           GEN3 = 2) { # error term for cpeptide model - to add more info

  output_log <- list()  # List to store messages, warnings, and errors

  #inputfile<-"test_data.csv"
  #load in file
  data <- rio::import(inputfile)

  # Replace '-' with 'm' in all column names ie if there are negative signs in the variable names ie. -10
  colnames(data) <- gsub("-", "m", colnames(data))


  # If there was a fasted timepoint inputted in the funciton - Define fasted timepoint
  if (!is.null(fasted_timepoint) & model == "glucose") {
    fasted_timepoint<-as.character(gsub("-","",fasted_timepoint))
    glu_fasted <- paste0("glum", fasted_timepoint)
    ins_fasted <- paste0("insm", fasted_timepoint)
  } else if (!is.null(fasted_timepoint) & model == "cpeptide") {
    fasted_timepoint<-as.character(gsub("-","",fasted_timepoint))
    glu_fasted <- paste0("glum", fasted_timepoint)
    cpep_fasted <- paste0("cpepm", fasted_timepoint)
  }else if(is.null(fasted_timepoint)){
    glu_fasted<-NULL
    cpep_fasted<-NULL
    ins_fasted<-NULL
  }

  # Define expected column names based on conditions
  if (npoints == 5 & model == "glucose") {
    expected_columns <- c(
      "id",
      "age_at_visit",
      "bmi",
      "weight",
      "ins0",
      "ins30",
      "ins60",
      "ins90",
      "ins120",
      "glu0",
      "glu30",
      "glu60",
      "glu90",
      "glu120"
    )

    # Conditionally add glu_fasted and ins_fasted if they exist
    if (!is.null(glu_fasted)) {
      expected_columns <- c(expected_columns, glu_fasted)
    }
    if (!is.null(ins_fasted)) {
      expected_columns <- c(expected_columns, ins_fasted)
    }

  } else if (npoints == 5 & model == "cpeptide") {
    expected_columns <- c(
      "id",
      "age_at_visit",
      "bmi",
      "weight",
      "cpep0",
      "cpep30",
      "cpep60",
      "cpep90",
      "cpep120",
      "glu0",
      "glu30",
      "glu60",
      "glu90",
      "glu120"
    )

    # Conditionally add glu_fasted and cpep_fasted if they exist
    if (!is.null(glu_fasted)) {
      expected_columns <- c(expected_columns, glu_fasted)
    }
    if (!is.null(cpep_fasted)) {
      expected_columns <- c(expected_columns, cpep_fasted)
    }

  } else if (npoints == 7 & model == "glucose") {
    expected_columns <- c(
      "id",
      "age_at_visit",
      "bmi",
      "weight",
      "cpep0",
      "cpep10",
      "cpep20",
      "cpep30",
      "cpep60",
      "cpep90",
      "cpep120",
      "ins0",
      "ins10",
      "ins20",
      "ins30",
      "ins60",
      "ins90",
      "ins120",
      "glu0",
      "glu10",
      "glu20",
      "glu30",
      "glu60",
      "glu90",
      "glu120"
    )

    # Conditionally add glu_fasted and ins_fasted if they exist
    if (!is.null(glu_fasted)) {
      expected_columns <- c(expected_columns, glu_fasted)
    }
    if (!is.null(ins_fasted)) {
      expected_columns <- c(expected_columns, ins_fasted)
    }

  } else if (npoints == 7 & model == "cpeptide") {
    expected_columns <- c(
      "id",
      "age_at_visit",
      "bmi",
      "weight",
      "cpep0",
      "cpep10",
      "cpep20",
      "cpep30",
      "cpep60",
      "cpep90",
      "cpep120",
      "glu0",
      "glu10",
      "glu20",
      "glu30",
      "glu60",
      "glu90",
      "glu120"
    )

    # Conditionally add glu_fasted and cpep_fasted if they exist
    if (!is.null(glu_fasted)) {
      expected_columns <- c(expected_columns, glu_fasted)
    }
    if (!is.null(cpep_fasted)) {
      expected_columns <- c(expected_columns, cpep_fasted)
    }

  } else if (is.character(npoints) & !is.null(timepoints) & model == "glucose") {
    ins_list <- paste("ins", timepoints, sep = "")
    glu_list <- paste("glu", timepoints, sep = "")
    expected_columns <- c("id", "age_at_visit", "bmi", "weight", ins_list, glu_list)

    # Conditionally add glu_fasted and ins_fasted if they exist
    if (!is.null(glu_fasted)) {
      expected_columns <- c(expected_columns, glu_fasted)
    }
    if (!is.null(ins_fasted)) {
      expected_columns <- c(expected_columns, ins_fasted)
    }

  } else if (is.character(npoints) & !is.null(timepoints) & model == "cpeptide") {
    cpep_list <- paste("cpep", timepoints, sep = "")
    glu_list <- paste("glu", timepoints, sep = "")
    expected_columns <- c("id", "age_at_visit", "bmi", "weight", cpep_list, glu_list)

    # Conditionally add glu_fasted and cpep_fasted if they exist
    if (!is.null(glu_fasted)) {
      expected_columns <- c(expected_columns, glu_fasted)
    }
    if (!is.null(cpep_fasted)) {
      expected_columns <- c(expected_columns, cpep_fasted)
    }
  }

  # Check if the data frame has all expected column names
  missing_columns <- setdiff(expected_columns, names(data))

  # Stop function execution if columns are missing
  if (length(missing_columns) > 0) {
    error_msg <- paste("Missing columns:", paste(missing_columns, collapse = ", "))
    output_log <- c(output_log, error_msg)
    stop(error_msg)
  }


  #identify rows with impossible demographic data ie. negative
  negative_rows <- data[data$age_at_visit <= 0 |
                          data$weight <= 0 | data$bmi <= 0, ]

  # Issue a warning if any negative values are found
  if (nrow(negative_rows) > 0) {
    warning_msg <- paste("IDs with invalid age, weight, or BMI:", paste(negative_rows$id, collapse = ", "))
    output_log <- c(output_log, warning_msg)
    data <- data[!(data$age_at_visit <= 0 | data$weight <= 0 | data$bmi <= 0), ]
  }


  # Identify rows with any missing data
  # Identify rows with any missing data
  missing_rows <- data[rowSums(is.na(data)) > 0, ]
  if (nrow(missing_rows) > 0) {
    missing_data_msg <- paste("Rows with missing OGTT data found. IDs removed:", paste(missing_rows$id, collapse = ", "),
                              "\nNumber of rows removed:", nrow(missing_rows))
    output_log <- c(output_log, missing_data_msg)
    data <- stats::na.omit(data)
  }

  # Iterate over each row to create individual STU files
  for (i in 1:nrow(data)) {
    # Extract data for the current row id
    current_row <- data[i, ]

    if (model == "glucose") {
      #define timepoints
      if (npoints == 5) {
        timepoints <- c(0, 30, 60, 90, 120)
      } else if (npoints == 7) {
        timepoints <- c(0, 10, 20, 30, 60, 90, 120)
      } else if (is.character(npoints) & !is.null(timepoints)) {
        timepoints <- timepoints
      }

      # Calculate constants
      bw <- current_row$weight
      # Identify columns that contain "glum" or "glu0"
      # Calculate the mean of these columns
      gss <- mean(unlist(current_row[grep("glum|glu0", names(current_row), value = TRUE)]), na.rm = TRUE)

      # Identify columns that contain "insm" or "ins0"
      # Calculate the mean of these columns
      iss <- mean(unlist(current_row[grep("insm|ins0", names(current_row), value = TRUE)]), na.rm = TRUE)

      D <- ifelse(bw < 75, glucose_protocol * bw * 1000, 75000)

      # Generate file name based on ID
      output_file <- file.path(outputdir, paste0(current_row$id, "_glucose.txt"))

      # Open a connection to write the STU file
      file_conn <- file(output_file, "w")

      # Write each line of the model configuration - this will likely be different for a 5 point or 7 point test
      # i could maybe have this as an enter in custom model config if needed
      writeLines(
        c(
          'STUDY      GLUCOSEMM', #what to put here
          'MODEL',
          'COMPDEF',
          '  CALCINTS 80',
          '  MAXITER 30',
          '  ABS_DATAWT 1',
          '  MDL_VARIANCE 0',
          '  INTGRTR Rosenbrock',
          '  INTGRTR_ERR_ON 1',
          '  INTGRTR_ERR 1.00000000e-03',
          '  CONV_CRIT 1.00000000e-05',
          '  BAYSIAN_TERM 1',
          '  LAMBDA 2.00000000e-01',
          'COMPART 3',
          '  EXPER MODEL',
          '  XCOORD 353',
          '  YCOORD 91',
          'COMPART q3',
          '  EXPER tracer',
          '  XCOORD 353',
          '  YCOORD 91',
          'COMPART 2',
          '  EXPER MODEL',
          '  XCOORD 207',
          '  YCOORD 91',
          'COMPART q2',
          '  EXPER tracer',
          '  XCOORD 207',
          '  YCOORD 91',
          '  FFTYPE EQ',
          '  FFDATA i',
          '  FFEQ q2.FF = lin(I)-Ib',
          'COMPART q1',
          '  EXPER tracer',
          '  XCOORD 93',
          '  YCOORD 165',
          'COMPART 1',
          '  EXPER MODEL',
          '  XCOORD 93',
          '  YCOORD 165',
          'XFER k(3,2)',
          '  EXPER MODEL',
          '  FROM 2',
          '  TO 3',
          'XFER k(3,2)',
          '  UNITS mass/time',
          '  EXPER tracer',
          '  FROM q2',
          '  TO q3',
          '  EQ flux(3,2) = k(3,2) * q2',
          '  EQ k(3,2)=SI*p2',
          'LOSS k(0,3)',
          '  EXPER MODEL',
          '  FROM 3',
          'LOSS k(0,3)',
          '  UNITS mass/time',
          '  EXPER tracer',
          '  FROM q3',
          '  EQ flux(0,3) = k(0,3) * q3',
          '  EQ k(0,3)=p2',
          'LOSS k(0,1)',
          '  UNITS mass/time',
          '  EXPER tracer',
          '  FROM q1',
          '  EQ flux(0,1) = k(0,1) * q1',
          '  EQ k(0,1)=p1+q3',
          'LOSS k(0,1)',
          '  EXPER MODEL',
          '  FROM 1',
          'SAMPLE s1',
          '  XCOORD 172',
          '  YCOORD 137',
          '  EXPER tracer',
          '  INCLUDE q1',
          '  LINK.TO Glucose',
          '  EQ s1 = q1/V',
          'EXOG.IN ex1',
          '  XCOORD 60',
          '  YCOORD 198',
          '  EXPER tracer',
          '  INCLUDE q1',
          '  TRACER EQUATION',
          '    I.BEG 0.00000000e+00',
          '    I.DUR 1.20000000e+02',
          '    E.EQ ex1 = p1*Gb*V',
          '  TRACER EQUATION',
          '    I.BEG 0.00000000e+00',
          '    I.DUR 1.00000000e+01',
          '    E.EQ ex1 = k1/(10-0)*(t-0)',
          '  TRACER EQUATION',
          '    I.BEG 1.00000000e+01',
          '    I.DUR 2.00000000e+01',
          '    E.EQ ex1 = k1+(k2-k1)/(30-10)*(t-10)',
          '  TRACER EQUATION',
          '    I.BEG 3.00000000e+01',
          '    I.DUR 3.00000000e+01',
          '    E.EQ ex1 = k2+(k3-k2)/(60-30)*(t-30)',
          '  TRACER EQUATION',
          '    I.BEG 6.00000000e+01',
          '    I.DUR 3.00000000e+01',
          '    E.EQ ex1 = k3+(k4-k3)/(90-60)*(t-60)',
          '  TRACER EQUATION',
          '    I.BEG 9.00000000e+01',
          '    I.DUR 3.00000000e+01',
          '    E.EQ ex1 = k4+(k5-k4)/(120-90)*(t-90)',
          '  TRACER EQUATION',
          '    I.BEG 1.20000000e+02',
          '    I.DUR 1.80000000e+02',
          '    E.EQ ex1 = k5*exp(-(t-120)/T)',
          'CHANGE',
          '  TYPE INSTANTANEOUS',
          '  START 0.00000000e+00',
          '  INTRVL 0.00000000e+00',
          '  EQ q1=Gb*V',
          'EXPERIMENT MODEL',
          '  TYPE MODEL',
          '  SUFFIX 0',
          'EXPERIMENT tracer',
          '  TYPE EXOGENOUS',
          '  SUFFIX 0',
          'EVENTLINE t',
          '  UNITS minutes',
          '  E.BEG 0.00000000e+00',
          '  E.FIN 1.80000000e+02',
          'GLOBL.EQ G = Glucose',
          'GLOBL.EQ GE=p1*V',
          'GLOBL.EQ k3=(D/BW*f-k1*15-k2*25-k4*30-k5*(15+T))/30',
          'GLOBL.EQ Ra= ex1.infusion-p1*Gb*V',
          'GLOBL.EQ p2=pp^2',
          'PARAM SI',
          '  VALUE 9.00000000e-04',
          '  LOWLIM 0.00000000e+00',
          '  HILIM 1.00000000e+01',
          'PARAM T',
          '  VALUE 8.00000000e+01',
          '  LOWLIM 4.00000000e+01',
          '  HILIM 1.25000000e+03',
          '  ADJUST 0',
          'PARAM V',
          '  VALUE 1.45000000e+00',
          '  LOWLIM 1.45000000e-01',
          '  HILIM 2.34600000e+01',
          '  ADJUST 0',
          'PARAM f',
          '  VALUE 9.00000000e-01',
          '  LOWLIM 8.00000000e-02',
          '  HILIM 8.00000000e+00',
          '  ADJUST 0',
          'PARAM k1',
          '  VALUE 1.10000000e+01',
          '  LOWLIM 0.00000000e+00',
          '  HILIM 2.00000000e+01',
          'PARAM k2',
          '  VALUE 1.00000000e+01',
          '  LOWLIM 8.00000000e-01',
          '  HILIM 8.00000000e+01',
          'PARAM k4',
          '  VALUE 1.10000000e+01',
          '  LOWLIM 0.00000000e+00',
          '  HILIM 5.00000000e+01',
          'PARAM k5',
          '  VALUE 1.00000000e+00',
          '  LOWLIM 0.00000000e+00',
          '  HILIM 3.00000000e+01',
          'PARAM p1',
          '  VALUE 2.50000000e-02',
          '  LOWLIM 4.10000000e-03',
          '  HILIM 4.10000000e-01',
          '  ADJUST 0',
          'PARAM pp',
          '  VALUE 1.00000000e-01',
          '  LOWLIM 1.00000000e-02',
          '  HILIM 1.00000000e+00',

          '  ADJUST 2',
          '  POPMEAN 1.00000000e-01',
          '  STDDEV 1.00000000e-02',
          'NOTES ex1 = k1/10*t',
          'ex1 = k1+(k2-k1)/(30-10)*(t-10)',
          'ex1 = k2+(k3-k2)/(60-30)*(t-30)',
          'ex1 = k3+(k4-k3)/(90-60)*(t-60)',
          'ex1 = k4+(k5-k4)/(120-90)*(t-90)',
          'ex1 = k5+(k6-k5)/(180-120)*(t-120)',
          'ex1 = k6+(k7-k6)/(240-180)*(t-180)~',
          'END',
          'STARTDATA'
        ),
        file_conn
      )

      writeLines("DATA", file_conn)
      writeLines(paste0("t G ", "FSD (", FSD, ") I"), file_conn)

      writeLines(paste("0", gss, iss), file_conn)

      # Write glucose and insulin values at each timepoint
      for (tp in timepoints[timepoints > 0]) {
        glu_value <- current_row[[paste0("glu", tp)]]
        ins_value <- current_row[[paste0("ins", tp)]]
        writeLines(paste(tp, glu_value, ins_value), file_conn)
      }


      # End data section
      writeLines("END", file_conn)

      # Write constants
      writeLines(paste("CONST BW", bw), file_conn)
      writeLines(paste("CONST Gss", gss), file_conn)
      writeLines(paste("CONST Iss", iss), file_conn)
      writeLines(paste("CONST D", D), file_conn)

      # is DG DATA neccessary for glucose model?

      writeLines("", file_conn) # Final blank line

      # Close the file connection
      close(file_conn)

      output_log <- c(output_log, paste(".stu file created successfully for Glucose Model, ID:", current_row$id))

      ## for cpeptide model different equations and constants are needed
    } else if (model == "cpeptide") {

      #define timepoints
      if (npoints == 5) {
        timepoints <- c(0, 30, 60, 90, 120)
      } else if (npoints == 7) {
        timepoints <- c(0, 10, 20, 30, 60, 90, 120)
      } else if (is.character(npoints) & !is.null(timepoints)) {
        timepoints <- timepoints
      }

      # Calculate constants
      bmi <- current_row$bmi
      bw <- current_row$weight
      height <- sqrt(bw / bmi)
      age <- current_row$age_at_visit
      bsa <- 0.016667 * bw ^ 0.5 * height ^ 0.5 #Mosteller formula # needed?
      gb <- mean(unlist(current_row[grep("glum|glu0", names(current_row), value = TRUE)]), na.rm = TRUE)
      cpb <- mean(unlist(current_row[grep("cpepm|cpep0", names(current_row), value = TRUE)]), na.rm = TRUE)
      Gmax <- do.call(pmax, c(dplyr::select(current_row, starts_with("glu")), na.rm = TRUE))
      # conditional obese params
      if (bmi > 30) {
        a1 <- 0.152
        FRA <- 0.78
      } else if (bmi <= 30){
        a1 <- 0.140
      FRA <- 0.76
      }

      maxtime <- max(timepoints)
      #extract glucose list using the pre derived basal as the first value
      glucose <-  as.numeric(unlist(c(gb,current_row[ , grep("^glu(?!m|0)", names(current_row), value = TRUE, perl = TRUE)])))

      #glucose first derivative- using modified akima as in Matlab and backwards difference. tested and results are same with this version compared to matlab.
      # Set the time interval
      dt <- 1
      new_t <- seq(timepoints[1], maxtime, by = dt)

      #this is the interpolation step - I have interpolated here to get the new glucose using modified akima as you previously had- I have tested and get the same values compared to matlab
      # this function also can compute the first derivaive
      new_g<-interpolators::evalInterpolator(interpolators::iprMakima(x=timepoints, y=glucose),new_t, derivative = 0)

      # Calculate backward differences
      d_back <- numeric(length(new_t))
      for (j in 2:length(new_t)) {
        d_back[j] <- (new_g[j] - new_g[j - 1]) / (new_t[j] - new_t[j - 1])
      }

      # Return the results
      t_dG <- new_t
      dG <- d_back

      # Generate file name based on ID
      output_file <- file.path(outputdir, paste0(current_row$id, "_cpeptide.txt"))

      # Open a connection to write the STU file
      file_conn <- file(output_file, "w")

      # Write the new block of text as soon as the file connection is opened
      writeLines(c(
        'STUDY		CPEPMM', #what to put here
        'MODEL',
        'COMPDEF',
        '  CALCINTS		20',
        '  MAXITER		20',
        '  ABS_DATAWT	1',
        '  MDL_VARIANCE	0',
        '  INTGRTR		Runge-Kutta',
        '  INTGRTR_ERR_ON	1',
        '  INTGRTR_ERR	1.00000000e-04',
        '  CONV_CRIT		1.00000000e-05',
        '  BAYSIAN_TERM	1',
        '  LAMBDA		1.00000000e+01',
        'COMPART		3',
        '  REF		Y',
        '  EXPER		MODEL',
        '  XCOORD	225',
        '  YCOORD	203',
        'COMPART		q3',
        '  REF		Y',
        '  EXPER		tracer',
        '  XCOORD	225',
        '  YCOORD	203',
        'COMPART		q1',
        '  REF		Cp1',
        '  EXPER		tracer',
        '  XCOORD	423',
        '  YCOORD	204',
        'COMPART		q2',
        '  REF		Cp2',
        '  EXPER		tracer',
        '  XCOORD	581',
        '  YCOORD	205',
        'COMPART		q4',
        '  REF		G-h',
        '  UNITS		mg/dl',
        '  EXPER		tracer',
        '  XCOORD	95',
        '  YCOORD	204',
        '  FFTYPE	EQ',
        '  FFEQ		q4.FF=(((beta*(G-h)+SRb)+abs(SRb+beta*(G-h)))/2)',
        'COMPART		q5',
        '  REF		dg/dt',
        '  UNITS		mg/(dl*min)',
        '  EXPER		tracer',
        '  XCOORD	422',
        '  YCOORD	66',
        '  FFTYPE	EQ',
        '  FFDATA	der',
        '  FFEQ		q5.FF = derivata',
        'COMPART		5',
        '  REF		dg/dt',
        '  EXPER		MODEL',
        '  XCOORD	422',
        '  YCOORD	66',
        'COMPART		4',
        '  REF		G-h',
        '  EXPER		MODEL',
        '  XCOORD	95',
        '  YCOORD	204',
        'COMPART		2',
        '  REF		Cp2',
        '  EXPER		MODEL',
        '  XCOORD	581',
        '  YCOORD	205',
        'COMPART		1',
        '  REF		Cp1',
        '  EXPER		MODEL',
        '  XCOORD	423',
        '  YCOORD	204',
        'XFER		k(3,4)',
        '  EXPER		MODEL',
        '  FROM		4',
        '  TO		3',
        'XFER		k(3,4)',
        '  UNITS		mass/time',
        '  EXPER		tracer',
        '  FROM		q4',
        '  TO		q3',
        '  EQ		flux(3,4) = k(3,4) * q4',
        '  EQ		k(3,4)=alpha',
        'XFER		k(2,1)',
        '  UNITS		mass/time',
        '  EXPER		tracer',
        '  FROM		q1',
        '  TO		q2',
        '  EQ		flux(2,1) = k(2,1) * q1',
        'XFER		k(1,2)',
        '  UNITS		mass/time',
        '  EXPER		tracer',
        '  FROM		q2',
        '  TO		q1',
        '  EQ		flux(1,2) = k(1,2) * q2',
        'XFER		k(1,2)',
        '  EXPER		MODEL',
        '  FROM		2',
        '  TO		1',
        'XFER		k(2,1)',
        '  EXPER		MODEL',
        '  FROM		1',
        '  TO		2',
        'LOSS		k(0,3)',
        '  EXPER		MODEL',
        '  FROM		3',
        'LOSS		k(0,3)',
        '  UNITS		mass/time',
        '  EXPER		tracer',
        '  FROM		q3',
        '  EQ		flux(0,3) = k(0,3) * q3',
        '  EQ		k(0,3)=alpha',
        'LOSS		k(0,1)',
        '  UNITS		mass/time',
        '  EXPER		tracer',
        '  FROM		q1',
        '  EQ		flux(0,1) = k(0,1) * q1',
        'LOSS		k(0,1)',
        '  EXPER		MODEL',
        '  FROM		1',
        'SAMPLE		s1',
        '  UNITS		pmol/l',
        '  XCOORD	447',
        '  YCOORD	149',
        '  EXPER		tracer',
        '  INCLUDE	q1',
        '  LINK.TO	Cpeptide',
        '  EQ		s1 = q1',
        'EXOG.IN		ex1',
        '  XCOORD	321',
        '  YCOORD	303',
        '  EXPER		tracer',
        '  INCLUDE	q1',
        '  TRACER	EQUATION',
        '    I.BEG	0.00000000e+00',
        '    I.DUR	1.20000000e+02',
        '    E.EQ	ex1 = SR',
        'CHANGE',
        '  TYPE		INSTANTANEOUS',
        '  START		0.00000000e+00',
        '  INTRVL	0.00000000e+00',
        '  EQ		q2=Cpb*k(2,1)/k(1,2)',
        'CHANGE',
        '  TYPE		INSTANTANEOUS',
        '  START		0.00000000e+00',
        '  INTRVL	0.00000000e+00',
        '  EQ		q1=Cpb',
        'CHANGE',
        '  TYPE		INSTANTANEOUS',
        '  START		0.00000000e+00',
        '  INTRVL	0.00000000e+00',
        '  EQ		q3=SRb',
        'EXPERIMENT	MODEL',
        '  TYPE		MODEL',
        '  SUFFIX	0',
        'EXPERIMENT	tracer',
        '  TYPE		EXOGENOUS',
        '  SUFFIX	0',
        'EVENTLINE	t',
        '  UNITS		minutes',
        '  E.BEG		0.00000000e+00',
        '  E.FIN		1.20000000e+02',
        'GLOBL.EQ	h=hperc*Gb',
        'GLOBL.EQ	# Formulas for kij from Van Cauter',
        'GLOBL.EQ	b1=0.69315/(0.14*age+29.16)',
        'GLOBL.EQ	k(1,2)=FRA*b1+(1-FRA)*a1',
        'GLOBL.EQ	k(0,1)=(a1*b1)/k(1,2)',
        'GLOBL.EQ	k(2,1)=a1+b1-k(1,2)-k(0,1)',
        'GLOBL.EQ	BSA=0.007194*(H^(0.725))*(BW^(0.425))',
        'GLOBL.EQ	#Volume WOMEN',
        'GLOBL.EQ	V=1.11*BSA+2.04',
        'GLOBL.EQ	#Volume MEN',
        'GLOBL.EQ	#V=1.92*BSA+0.64',
        'GLOBL.EQ	G=lin(Glucose)',
        'GLOBL.EQ	d=(lin(dG)+abs(lin(dG)))/2',
        'GLOBL.EQ	derivata=d*((G-Gb)+abs(G-Gb))/(2*(G-Gb)+0.00001)',
        'GLOBL.EQ	SRb=k(0,1)*Cpb',
        'GLOBL.EQ	SRd=K*q5.FF',
        'GLOBL.EQ	SRs=q3',
        'GLOBL.EQ	SR=(SRs+SRd)',
        'GLOBL.EQ	ISR=(SR+SRb)*V',
        'GLOBL.EQ	# Metabolic indices',
        'GLOBL.EQ	phib=(SRb)/(Gb*0.05551)',
        'GLOBL.EQ	phid=K/0.05551',
        'GLOBL.EQ	phis=beta/0.05551',
        'GLOBL.EQ	phitotOB=(K*deltaG*18.016)/(aucG-h*T)+phis',
        'PARAM		K',
        '  VALUE		1.55845733e+01',
        '  LOWLIM	0.00000000e+00',
        '  HILIM		9.00000000e+01',
        'PARAM		a',
        '  VALUE		0.00000000e+00',
        '  LOWLIM	0.00000000e+00',
        '  HILIM		1.00000000e+00',
        '  ADJUST	0',
        'PARAM		alpha',
        '  VALUE		7.36148366e-02',
        '  LOWLIM	5.00000000e-03',
        '  HILIM		5.00000000e-01',
        '  ADJUST	2',
        '  POPMEAN	9.00000000e-02',
        '  STDDEV	4.50000000e-02',
        'PARAM		b',
        '  VALUE		0.00000000e+00',
        '  LOWLIM	0.00000000e+00',
        '  HILIM		1.00000000e+01',
        '  ADJUST	0',
        'PARAM		beta',
        '  VALUE		3.02072239e+00',
        '  LOWLIM	0.00000000e+00',
        '  HILIM		1.00000000e+01',
        'PARAM		hperc',
        '  VALUE		1.27867805e+00',
        '  LOWLIM	8.00000000e-01',
        '  HILIM		1.50000000e+00',
        '  ADJUST	2',
        '  POPMEAN	1.00000000e+00',
        '  STDDEV	1.00000000e-01',
        'END'
      ), file_conn)

      ## in the C-peptide old file these are also in the text string are they necessary?- H, BMI a1 and FRA should be derived per individual ?
      # Also where do you derived aucGLP1,aucGLPmax,deltaG,aucG and aucGLP1
      # the relevant constants for the C-peptide model i have specified below but may need editing

      # 'CONST	H	138.9' newline...
      # 'CONST	BMI	16.1' newline...
      # 'CONST	GLP1b	0.001' newline...
      # 'CONST	a1	0.14' newline...
      # 'CONST	FRA	0.76' newline...
      # 'CONST	GLP1max	0.001' newline...
      # 'CONST	deltaG	59' newline...
      # 'CONST	T	120' newline...
      # 'CONST	aucG	20245' newline...
      # 'CONST	aucGLP1	0.001' newline...

      #something also to note is the above - we dont feed in Sex as a variable - shoud we be doing this or is this to calculate generalized kinetics assuming a max BSA for women and max for men?
      # 'GLOBL.EQ	BSA=0.007194*(H^(0.725))*(BW^(0.425))',
      # 'GLOBL.EQ	#Volume WOMEN',
      # 'GLOBL.EQ	V=1.11*BSA+2.04',
      # 'GLOBL.EQ	#Volume MEN',
      # 'GLOBL.EQ	#V=1.92*BSA+0.64',

      #Write header
      writeLines("DATA", file_conn)
      writeLines(paste0("t G ", "GEN (", GEN1, " ", GEN2, " ", GEN3, ") C"),
                 file_conn)

      writeLines(paste0("0", gb, cpb), file_conn)

      # Write glucose and cpep values at each timepoint
      for (tp in timepoints[timepoints > 0]) {
        glu_value <- current_row[[paste0("glu", tp)]]
        cpep_value <- current_row[[paste0("cpep", tp)]]
        writeLines(paste(tp, glu_value, cpep_value), file_conn)
      }

      # End data section
      writeLines("END", file_conn)

      # Write constants
      writeLines(paste("CONST BW", bw), file_conn)
      writeLines(paste("CONST AGE", age), file_conn)
      writeLines(paste("CONST BMI", bmi), file_conn)
      writeLines(paste("CONST BSA", bsa), file_conn)
      writeLines(paste("CONST Gmax", Gmax), file_conn)
      #writeLines(paste("CONST aucG", aucG), file_conn) # see query above
      writeLines(paste("CONST Gb", gb), file_conn)
      writeLines(paste("CONST Cpb", cpb), file_conn)
      writeLines(paste("CONST a1", a1), file_conn)
      writeLines(paste("CONST FRA", FRA), file_conn)
      writeLines(paste("CONST T", maxtime), file_conn)
      writeLines("#", file_conn)
      writeLines("DATA", file_conn)

      # add the glucose derivative
      writeLines("t dG GEN (SD 2)",file_conn)

      for (d in seq_along(new_t)) {
        line <- paste(t_dG[d], dG[d])
        writeLines(line, file_conn)
      }

      # Close the file connection
      # Close the file connection
      close(file_conn)

      output_log <- c(output_log, paste(".stu file created successfully for C-peptide Model, ID:", current_row$id))
    }

  }
  return(output_log)
}
