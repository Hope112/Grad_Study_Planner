# Graduate Study Planner (R Shiny)

A modern, minimal **R Shiny** app for planning, tracking, and exporting your graduate coursework. Add courses by term, tag them by category (e.g., *Core*, *Methods*, *Elective*), monitor GPA and progress, auto-infer milestones, and export a Plan of Study to Word.

## ✨ Key Features

- **Course planner:** Add/edit courses with semester, year, credits, completion, grade, and categories (tags).
- **Analytics dashboard:** 
  - Completed vs. planned credits by term  
  - Credit distribution by category  
  - Academic timeline (cumulative completed vs. total planned)  
  - Semester GPA trend with smoothing  
  - Category-level performance table
- **Progress metrics:** Completion rate, average credits/semester, estimated graduation.
- **Import/Export:** CSV import (with validation) and **Word** export (Plan of Study grouped by category).
- **Autosave:** State is saved to your browser’s `localStorage` (no server DB required).
- **Timeline helper:** Auto-infer major milestones from your entered data; editable in the app.

---

## 📦 Requirements

- R (≥ 4.2 recommended)
- The following CRAN packages:
  - `shiny`, `shinydashboard`, `shinyjs`, `shinyWidgets`, `DT`
  - `ggplot2`, `dplyr`, `tidyr`, `scales`, `stringr`
  - `jsonlite`
  - `officer`, `flextable`

Install (first time only):

```r
install.packages(c(
  "shiny","shinydashboard","shinyjs","DT","shinyWidgets",
  "ggplot2","dplyr","tidyr","scales","jsonlite","officer","flextable","stringr"
))
```

---

## 🚀 Run the App

Clone the repo and run:

```r
shiny::runApp()
```

or open the `app.R` file in RStudio and click **Run App**.

---

## 🗂️ Data Model

The app stores courses in a table with these columns:

| Column         | Type       | Description                                           |
|----------------|------------|-------------------------------------------------------|
| `Semester`     | character  | One of **Spring**, **Summer**, **Fall**              |
| `CalendarYear` | integer    | Year (e.g., 2025)                                     |
| `Course`       | character  | Course title (e.g., *Research Methods*)               |
| `Code`         | character  | Course code (e.g., *ERM 601*)                         |
| `Credit`       | numeric    | Credit hours (e.g., 3)                                |
| `Completed`    | logical    | `TRUE` if finished (auto-set when a `Grade` is entered) |
| `Grade`        | character  | One of: A, A-, B+, B, B-, C+, C, C-, F                |
| `Tags`         | list/array | Categories (e.g., *Core; Methods; Elective*)          |

> **Note:** On **CSV import**, `Completed` is **inferred** from `Grade`. If a valid grade exists, the course is considered completed.

---

## 📥 Importing Data (CSV)

Use **Import Data** in the sidebar. Your CSV should have these columns (header row required):

```
Semester,CalendarYear,Course,Code,Credit,Grade,Tags
Spring,2025,Research Methods,ERM601,3,A,"Core; Methods"
Fall,2025,Item Response Theory,ERM720,3,,"Core; Theory"
```

- `Semester` must be **Spring**, **Summer**, or **Fall** (case-insensitive is handled).
- `Tags` can be separated by commas or semicolons (e.g., `"Core; Methods"`).
- `Grade` can be blank for in-progress courses.
- `Completed` is **not** required in the CSV—it's inferred from `Grade`.

Download a blank **Template** CSV from the sidebar at any time.

---

## 📤 Exporting

- **Export CSV:** Your current table (including tags) for backup or external analysis.
- **Export to Word:** Creates a **Plan of Study** (`.docx`) grouped by category, with a **Summary** and a **Projected Timeline** table (admit, hours completed, coursework, comps, proposal, residency, dissertation, notes).

---

## 📊 Analytics Overview

- **Academic Progress Overview:** Stacked bars of completed vs. pending credits per term.  
- **Credit Distribution by Category:** Bar chart of total credits per tag.  
- **Academic Timeline:** Cumulative completed vs. total planned credits across semesters.  
- **Performance Trends:** Semester GPA (size = credits), target lines (3.0, 3.5), and LOESS trend.  
- **Detailed Performance Analysis:** Per-category courses, credits, avg GPA, and completion status.

---

## 🧭 Timeline & Milestones

Under **Export & Timeline**, you can set or auto-infer:

- Date Admitted, Hours Completed, Coursework Complete, Comprehensive Exam, Proposal, Residency Semesters, Dissertation Complete, Notes.

Click **Auto-Fill** to infer from current data (you can edit any field afterward).

---

## 💾 Persistence & Reset

- The app **autosaves** to your browser (`localStorage`).  
- Use **Reset All Data** (Dashboard → *Danger Zone*) to clear the local state.

> **Tip:** If you work across devices or browsers, use **Export CSV** regularly for backups.

---

## 🛠️ Troubleshooting

- **Import failed: Missing columns…**  
  Ensure your CSV headers exactly match: `Semester,CalendarYear,Course,Code,Credit,Grade,Tags`.

- **Grades not recognized / GPA not updating**  
  Grades must be one of: `A, A-, B+, B, B-, C+, C, C-, F`. Any valid grade marks a course **Completed**.

- **Semester order looks odd**  
  Use the built-in `Spring / Summer / Fall` options and valid years (e.g., 2023–2030).

- **Autosave error**  
  Your browser may block or be out of `localStorage` space. Export CSV to back up, then clear some local storage or use a different browser profile.

- **Word export says “No data to export.”**  
  Add at least one course (or import via CSV) before exporting.

---

## 🔐 Privacy

All data is stored **locally in your browser** unless you explicitly export it.

---

## 📄 License

MIT license.  

---
