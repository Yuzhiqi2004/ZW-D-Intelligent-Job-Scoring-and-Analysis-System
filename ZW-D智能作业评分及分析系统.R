library(shiny)
library(httr)
library(stringr)
library(DT)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(shinythemes)
library(shinycssloaders)
library(readtext)
library(tools)

# DeepSeek API配置
API_URL <- "https://api.deepseek.com/v1/chat/completions"

# 自定义评分函数
evaluate_assignment <- function(api_key, content, criteria) {
  prompt <- paste0(
    "请根据以下评分标准对作业进行评分：\n",
    criteria, "\n\n",
    "作业内容：\n", content,
    "\n\n请严格按以下格式返回评分结果：\n",
    "分数：x/100\n",
    "评分依据：（分点说明，每个评分项换行显示）\n",
    "按评分标准顺序逐项说明得分/评语"
  )
  
  # 优化API调用参数
  response <- POST(
    API_URL,
    add_headers(
      `Content-Type` = "application/json",
      `Authorization` = paste("Bearer", api_key)
    ),
    body = list(
      model = "deepseek-chat",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3,
      max_tokens = 1024,
      top_p = 0.9
    ),
    encode = "json",
    timeout(30)
  )
  
  # 错误处理机制
  if (response$status_code != 200) {
    return(paste("API请求失败:", response$status_code, content(response, "text")))
  }
  
  content <- content(response, "parsed")
  return(content$choices[[1]]$message$content)
}

# DeepSeek成绩分析函数
generate_grade_analysis <- function(api_key, score_data) {
  # 构建五级成绩分布
  five_level_data <- score_data %>%
    mutate(
      level = case_when(
        数值分数 >= 90 ~ "优秀",
        数值分数 >= 80 & 数值分数 < 90 ~ "良好",
        数值分数 >= 70 & 数值分数 < 80 ~ "中等",
        数值分数 >= 60 & 数值分数 < 70 ~ "及格",
        数值分数 < 60 ~ "不及格"
      ),
      level = factor(level, levels = c("优秀", "良好", "中等", "及格", "不及格"))
    ) %>%
    count(level, .drop = FALSE) %>%
    mutate(percent = n / sum(n) * 100)
  
  # 创建数据摘要
  data_summary <- paste0(
    "总人数: ", nrow(score_data), "\n",
    "平均分: ", round(mean(score_data$数值分数), 1), "\n",
    "最高分: ", max(score_data$数值分数), "\n",
    "最低分: ", min(score_data$数值分数), "\n",
    "成绩分布:\n",
    paste0(five_level_data$level, ": ", five_level_data$n, "人 (", 
           round(five_level_data$percent, 1), "%)", collapse = "\n")
  )
  
  # 构建分析提示词
  prompt <- paste0(
    "你是一位教育数据分析专家，请根据以下学生成绩分布数据进行全面分析：\n",
    "### 成绩数据摘要 ###\n",
    data_summary,
    "\n\n### 分析要求 ###\n",
    "1. 分析整体分布特征（正态/偏态分布）\n",
    "2. 识别优势学科和薄弱环节\n",
    "3. 指出异常分布或两极分化现象\n",
    "4. 提出针对性教学建议\n",
    "5. 用简洁的学术语言输出\n",
    "6. 减少文字中非标点符号的使用\n"
  )
  
  # 调用DeepSeek API
  response <- POST(
    API_URL,
    add_headers(
      `Content-Type` = "application/json",
      `Authorization` = paste("Bearer", api_key)
    ),
    body = list(
      model = "deepseek-chat",
      messages = list(list(role = "user", content = prompt)),
      temperature = 0.3
    ),
    encode = "json",
    timeout(30)
  )
  
  content <- content(response, "parsed")
  return(content$choices[[1]]$message$content)
}

# 优化后的Shiny UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  title = "智能作业评分系统",
  
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .well { border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1); background-color: #f8f9fa; }
      .btn-primary { background-color: #3498db; border-color: #2980b9; }
      .btn-primary:hover { background-color: #2980b9; }
      .main-header h1 { background-color: #3498db; color: white; padding: 15px; border-radius: 8px; margin-top: 0; box-shadow: 0 2px 4px rgba(0,0,0,0.2); }
      .nav-tabs > li > a { color: #2c3e50; font-weight: bold; }
      .nav-tabs > li.active > a { border-bottom: 3px solid #3498db !important; color: #3498db !important; }
      .sidebar-title { color: #2c3e50; margin-top: 0; margin-bottom: 15px; font-weight: bold; border-bottom: 2px solid #3498db; padding-bottom: 8px; }
      .analysis-report { 
        background-color: #f8f9fa; 
        border-radius: 8px; 
        padding: 15px; 
        margin-top: 20px;
        border-left: 4px solid #3498db;
        white-space: pre-wrap;
        max-height: 300px; /* 新增：限制高度 */
        overflow-y: auto; /* 新增：添加滚动条 */
      }
      .report-header { 
        color: #2c3e50; 
        font-weight: bold; 
        margin-bottom: 10px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .upload-note {
        background-color: #e3f2fd;
        border-radius: 4px;
        padding: 8px 12px;
        margin-top: 10px;
        font-size: 13px;
        border-left: 3px solid #3498db;
      }
      .error-message {
        color: #e74c3c;
        font-weight: bold;
        margin-top: 5px;
      }
      .feedback-content {
        text-indent: 2em;
        margin-bottom: 0.5em;
        line-height: 1.6;
        display: block;
      }
      .action-button-group {
        display: flex;
        gap: 10px;
        margin-top: 15px;
      }
      .action-button-group .btn {
        flex: 1;
      }
      .graph-container {
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(class = "main-header",
      titlePanel(div(icon("graduation-cap"), "ZW-D 智能作业评分及分析系统"))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      div(class = "well",
          h4(class = "sidebar-title", "评分参数设置"),
          passwordInput("api_key", 
                        label = div(icon("key"), "DeepSeek API密钥"), 
                        placeholder = "输入sk-开头的API密钥"),
          
          br(),
          
          h4(class = "sidebar-title", "评分标准设置"),
          textAreaInput("criteria", label = NULL,
                        value = paste(
                          "1. 检查是否使用tidyverse改写了原始代码（20分）\n",
                          "2. 确认是否包含R语言语法规则解释（30分）\n",
                          "3. 验证是否包含完整的数据结构解释（20分）\n",
                          "4. 检查结果解释是否充分（20分）\n",
                          "5. 若完全未涉及语法规则解释，则总分不得超过60分\n",
                          "6. 最低分不低于60分\n",
                          "评分规则：\n",
                          "- 每个评分项独立评分\n",
                          "- 总分自动求和"),
                        rows = 8,
                        placeholder = "输入评分标准...")
      ),
      
      div(class = "well",
          h4(class = "sidebar-title", "作业上传"),
          fileInput("files", 
                    label = div(icon("file-import"), "上传作业文件（可多选）"), 
                    multiple = TRUE, 
                    accept = c(".txt", ".R", ".rmd", ".qmd", ".py", ".pdf", ".doc", ".docx"),
                    buttonLabel = "浏览..."),
          
          div(class = "upload-note",
              icon("info-circle"), 
              "支持格式：R(.R), R Markdown(.rmd), Quarto(.qmd), Python(.py), PDF(.pdf), Word(.doc/.docx)"
          ),
          
          div(class = "upload-note",
              icon("info-circle"), 
              "文件名格式：学号+姓名（例如：20222101001张三.pdf）"
          ),
          
          br(),
          
          div(class = "action-button-group",
              actionButton("score", 
                           label = div(icon("play"), "开始评分"), 
                           class = "btn-primary",
                           width = "100%"),
              
              downloadButton("download", 
                             label = div(icon("download"), "下载结果"), 
                             class = "btn-success")
          )
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(div(icon("table"), "评分结果"), 
                 div(class = "well",
                     DTOutput("resultsTable") %>% 
                       shinycssloaders::withSpinner(type = 6, color = "#3498db")
                 )
        ),
        
        tabPanel(div(icon("chart-bar"), "可视化分析"),
                 fluidRow(
                   column(7, 
                          div(class = "well graph-container",
                              h4("学生分数分布", style = "text-align: center;"),
                              plotOutput("scoreBars", height = "400px") %>% 
                                shinycssloaders::withSpinner(type = 6, color = "#3498db")
                          )
                   ),
                   column(5,
                          div(class = "well graph-container",
                              h4("分数区间占比", style = "text-align: center;"),
                              plotOutput("scorePie", height = "350px") %>% 
                                shinycssloaders::withSpinner(type = 6, color = "#3498db"),
                              
                              div(style = "text-align: center; margin-top: 20px;",
                                  actionButton("analyze", 
                                               label = div(icon("brain"), "DeepSeek智能分析"), 
                                               class = "btn-info",
                                               width = "100%")
                              )
                          )
                   )
                 ),
                 
                 div(class = "well",
                     div(class = "report-header",
                         h4("成绩分析报告", style = "margin: 0;"),
                         downloadButton("downloadReport", "导出报告", class = "btn-sm")
                     ),
                     div(class = "analysis-report",
                         verbatimTextOutput("analysisReport")
                     )
                 )
        )
      )
    )
  ) 
)

# Shiny Server
server <- function(input, output) {
  results <- reactiveVal(data.frame(
    学号 = character(),
    姓名 = character(),
    分数 = character(),
    评分依据 = character(),
    错误信息 = character(),
    stringsAsFactors = FALSE
  ))
  
  # 存储分析报告
  analysis_report <- reactiveVal("分析报告将在此处显示...")
  
  # 文件内容提取函数
  extract_file_content <- function(file_path, file_ext) {
    tryCatch({
      # 文本类文件直接读取
      if (file_ext %in% c("txt", "r", "rmd", "qmd", "py")) {
        content <- paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
        return(content)
      } 
      # PDF文件处理
      else if (file_ext == "pdf") {
        pdf_text <- readtext::readtext(file_path)$text
        return(gsub("\\s+", " ", pdf_text))
      } 
      # Word文件处理
      else if (file_ext %in% c("doc", "docx")) {
        doc_text <- readtext::readtext(file_path)$text
        return(gsub("\\s+", " ", doc_text))
      } 
      # 其他格式尝试直接读取
      else {
        return(paste(readLines(file_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      }
    }, error = function(e) {
      return(paste0("文件读取错误: ", e$message))
    })
  }
  
  observeEvent(input$score, {
    req(input$files, input$api_key, input$criteria)
    
    # 创建进度条
    progress <- shiny::Progress$new()
    progress$set(message = "评分进行中", value = 0)
    on.exit(progress$close())
    
    responses <- list()
    for (i in 1:nrow(input$files)) {
      file <- input$files[i, ]
      filename <- file$name
      
      # 解析学号姓名（支持多种扩展名）
      file_ext <- tolower(tools::file_ext(filename))
      base_filename <- sub(paste0("\\.", file_ext, "$"), "", filename)
      
      student_id <- str_extract(base_filename, "^\\d+")
      student_name <- str_replace(base_filename, paste0("^", student_id), "")
      student_name <- str_trim(gsub("[^\\p{Han}]", "", student_name, perl = TRUE))
      
      # 根据文件类型提取内容
      content <- extract_file_content(file$datapath, file_ext)
      
      # 错误处理
      if (grepl("文件读取错误", content)) {
        responses[[i]] <- data.frame(
          学号 = ifelse(is.na(student_id), "未知", student_id),
          姓名 = ifelse(is.na(student_name), "未知", student_name),
          分数 = "ERROR",
          评分依据 = "",
          错误信息 = content,
          stringsAsFactors = FALSE
        )
        next
      }
      
      # 更新进度条
      progress$inc(1/nrow(input$files), detail = paste("评分中:", student_name, "(", file_ext, ")"))
      
      # API评分
      evaluation <- tryCatch({
        evaluate_assignment(input$api_key, content, input$criteria)
      }, error = function(e) {
        return(paste("评分错误：", e$message))
      })
      
      # 错误处理
      if (grepl("评分错误|API请求失败", evaluation)) {
        responses[[i]] <- data.frame(
          学号 = ifelse(is.na(student_id), "未知", student_id),
          姓名 = ifelse(is.na(student_name), "未知", student_name),
          分数 = "ERROR",
          评分依据 = "",
          错误信息 = evaluation,
          stringsAsFactors = FALSE
        )
        next
      }
      
      # 解析结果
      score_match <- str_match(evaluation, "分数：([0-9]+)/100")
      original_score <- ifelse(is.na(score_match[2]), 0, as.numeric(score_match[2]))
      raw_feedback <- str_replace(evaluation, "分数：[0-9]+/100\\n", "")
      
      # 格式化反馈 - 新增分段和缩进处理
      formatted_feedback <- raw_feedback %>%
        str_split("\n") %>% 
        unlist() %>% 
        str_replace_all("([0-9]+)\\..*?：", "🔸 评分项\\1：") %>%
        str_remove_all("[^\\p{So}\\p{L}\\p{N}\\p{P}\\p{Z}\\p{M}]") %>%
        .[. != ""] %>%  # 移除空行
        paste0("<div class='feedback-content'>", ., "</div>") %>%  # 添加CSS类
        paste(collapse = "")
      
      responses[[i]] <- data.frame(
        学号 = ifelse(is.na(student_id), "未知", student_id),
        姓名 = ifelse(is.na(student_name), "未知", student_name),
        分数 = paste0(original_score, "/100"),
        评分依据 = formatted_feedback,
        错误信息 = "",
        stringsAsFactors = FALSE
      )
    }
    
    results(bind_rows(responses))
    analysis_report("分析报告将在此处显示...")  # 重置分析报告
  })
  
  # 格式化数据
  formatted_data <- reactive({
    req(results())
    
    results() %>%
      mutate(
        数值分数 = as.numeric(str_extract(分数, "\\d+")),
        分数区间 = cut(
          数值分数, 
          breaks = seq(0, 100, by = 10),
          labels = paste0(seq(0, 90, by = 10), "-", seq(10, 100, by = 10)),
          include.lowest = TRUE
        )
      ) %>%
      filter(!is.na(数值分数) & 分数 != "ERROR")
  })
  
  # 结果表格
  output$resultsTable <- renderDT({
    data <- results()
    
    # 错误信息提示
    if (any(data$分数 == "ERROR")) {
      showNotification("部分作业评分失败，请检查错误信息列", type = "warning")
    }
    
    datatable(data,
              escape = FALSE,
              options = list(
                scrollX = TRUE,
                autoWidth = TRUE,
                columnDefs = list(
                  list(width = "80px", targets = 0),   # 学号
                  list(width = "100px", targets = 1),   # 姓名
                  list(width = "80px", targets = 2),   # 分数
                  list(width = "400px", targets = 3),   # 评分依据
                  list(width = "200px", targets = 4,    # 错误信息
                       className = "error-message",
                       render = JS("function(data, type, row) {",
                                   "return type === 'display' && data ?",
                                   "'<span class=\"error-message\">' + data + '</span>' : data;",
                                   "}"))
                ),
                language = list(
                  url = "//cdn.datatables.net/plug-ins/1.10.19/i18n/Chinese.json"
                ),
                pageLength = 10
              )) %>%
      formatStyle("评分依据", whiteSpace = "pre-wrap") %>%
      formatStyle(names(data), fontWeight = "bold")
  })
  
  # 柱状图
  output$scoreBars <- renderPlot({
    data <- formatted_data()
    if (nrow(data) == 0) return()
    
    ggplot(data, aes(x = reorder(姓名, 数值分数), y = 数值分数, fill = 数值分数)) +
      geom_col(width = 0.7, alpha = 0.8) +
      geom_text(aes(label = 数值分数), vjust = -0.5, size = 4, color = "#2c3e50", fontface = "bold") +
      scale_fill_gradient(low = "#5DADE2", high = "#1A5276", guide = "none") +
      labs(x = "学生姓名", y = "分数", 
           title = "学生作业分数分布") +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10)) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#2c3e50"),
        axis.title = element_text(size = 12, face = "bold"),
        panel.grid.major.y = element_line(color = "#ecf0f1"),
        panel.grid.major.x = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  # 饼状图
  output$scorePie <- renderPlot({
    data <- formatted_data() %>%
      count(分数区间, .drop = FALSE) %>%
      mutate(
        占比 = n / sum(n),
        占比标签 = ifelse(n > 0, 
                      paste0(分数区间, "\n", n, "人 (", percent(占比, accuracy = 1), ")"),
                      NA)
      )
    
    if (nrow(data) == 0) return()
    
    # 创建颜色映射
    all_intervals <- levels(data$分数区间)
    n_intervals <- length(all_intervals)
    colors <- colorRampPalette(c("#e74c3c", "#f1c40f", "#27ae60"))(n_intervals)
    names(colors) <- all_intervals
    
    # 过滤有数据的区间
    plot_data <- data %>% filter(n > 0)
    
    ggplot(plot_data, aes(x = "", y = n, fill = 分数区间)) +
      geom_bar(stat = "identity", width = 1, alpha = 0.9) +
      coord_polar(theta = "y") +
      geom_text(aes(label = 占比标签),
                position = position_stack(vjust = 0.5),
                size = 4.5, color = "white", fontface = "bold") +
      labs(fill = "分数区间") +
      scale_fill_manual(values = colors) +
      theme_void() +
      theme(
        legend.position = "right",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.background = element_rect(fill = "white", color = NA),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 12, face = "bold")
      )
  })
  
  # DeepSeek成绩分析
  observeEvent(input$analyze, {
    req(input$api_key, formatted_data())
    
    if (nrow(formatted_data()) == 0) {
      analysis_report("错误：无有效成绩数据可分析")
      return()
    }
    
    progress <- shiny::Progress$new()
    progress$set(message = "DeepSeek分析中...", value = 0.3)
    on.exit(progress$close())
    
    # 调用分析函数
    analysis_result <- tryCatch({
      generate_grade_analysis(input$api_key, formatted_data())
    }, error = function(e) {
      paste("分析失败:", e$message)
    })
    
    progress$set(value = 1)
    analysis_report(analysis_result)
  })
  
  # 渲染分析报告
  output$analysisReport <- renderText({
    analysis_report()
  })
  
  # 下载处理
  output$download <- downloadHandler(
    filename = function() {
      paste("作业评分结果-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_excel_csv(
        results() %>%
          mutate(评分依据 = str_replace_all(评分依据, "<div class='feedback-content'>|</div>", "")),
        file,
        na = ""
      )
    }
  )
  
  # 新增：导出分析报告
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("成绩分析报告-", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      writeLines(analysis_report(), file, useBytes = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)