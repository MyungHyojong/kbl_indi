!apt-get update
!apt-get install -y chromium-chromedriver
!cp /usr/lib/chromium-browser/chromedriver /usr/bin
!pip install selenium
! pip install folium
# Google Colab에서 Selenium을 사용하기 위한 설정
!pip install selenium
# ChromeDriver 설치
!apt-get update
!apt-get install -y chromium-chromedriver

# ChromeDriver 경로 설정
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import Select
from selenium.common.exceptions import NoSuchElementException
import time
from bs4 import BeautifulSoup
import re

chrome_options = webdriver.ChromeOptions()
chrome_options.add_argument('--headless')  # 화면 표시하지 않음
chrome_options.add_argument('--no-sandbox')
chrome_options.add_argument('--disable-dev-shm-usage')
chrome_options.add_argument('--disable-gpu')
chrome_options.add_argument('--disable-extensions')
chrome_options.add_argument('--disable-software-rasterizer')
chrome_options.add_argument('--remote-debugging-port=9222')  # 디버깅 포트 설정
chrome_options.add_argument('user-agent=Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36')

import pandas as pd
from bs4 import BeautifulSoup

def crawling(target_url):
  # ChromeDriver 옵션을 직접 설정하여 전달
  driver = webdriver.Chrome(options=chrome_options)
  driver.get(target_url)
  time.sleep(1)
  try:
      # "슛차트" 버튼이 화면에 나타날 때까지 기다림
      player_record_button = WebDriverWait(driver, 5).until(
          EC.visibility_of_element_located((By.CSS_SELECTOR, 'li[data-key="playerDetail"]'))
      )
      player_record_button.click()

      time.sleep(1)
      html_code = driver.page_source
      #print(html_code)
      print(target_url)
      return(html_code)

  except Exception as e:
      print("오류 예상")
      driver.quit()
      return 0


base_url = "https://kbl.or.kr/game/record/"

# URL마다 데이터를 크롤링하고 쌓아가기
for year in range(43, 44, 2):
  target_urls = [f"{base_url}S{str(year).zfill(2)}G01N{str(n).zfill(3)}"
        for n in range(1, 271)]
  all_data_df = pd.DataFrame()
  for target_url in target_urls:
      # HTML 데이터 로드
      # HTML 데이터 로드
      html_code = crawling(target_url)

      # HTML 코드가 제대로 받아졌는지 확인
      if isinstance(html_code, str):
          # BeautifulSoup으로 HTML 파싱
          soup = BeautifulSoup(html_code, 'html.parser')

          # 이후의 코드는 여기서 계속 진행
      else:
          print(f"Error: crawling function did not return valid HTML for {target_url}")
          print(f"Returned type: {type(html_code)} and value: {html_code}")

      # 경기 일자 추출
      game_date = soup.select_one('.bd-date').get_text(strip=True)

      # 홈팀과 원정팀 이름 추출
      team_headers = soup.select('.con-box .con-tit h4')
      home_team_name = team_headers[0].get_text(strip=True)
      away_team_name = team_headers[1].get_text(strip=True)

      # 홈팀 선수 이름 추출
      home_player_names = [name.get_text(strip=True) for name in soup.select('.archive-team-table01-wrap')[0].select('tbody .name')]

      # 원정팀 선수 이름 추출
      away_player_names = [name.get_text(strip=True) for name in soup.select('.archive-team-table01-wrap')[1].select('tbody .name')]

      # 필드명 정의 (선수 이름 포함)
      columns = ["Team", "Opponent", "Date", "Name", "MIN", "Pts", "2PT M/A", "2PT %", "3PT M/A", "3PT %", "FG M/A", "FG %",
                "FT M/A", "FT %", "OR", "DR", "TOT", "DK", "AST", "TO", "Stl", "BS", "PF", "FO", "PP"]

      # 홈팀 선수 기록 데이터 추출
      home_table = soup.select('.top-scroll-table')[0].select('tbody tr')
      home_data = []
      for i, row in enumerate(home_table):
          stats = [stat.get_text(strip=True) for stat in row.find_all('td')]
          if i < len(home_player_names):  # 선수 이름과 매칭
              home_data.append([home_team_name, away_team_name, game_date, home_player_names[i]] + stats)

      # 원정팀 선수 기록 데이터 추출
      away_table = soup.select('.top-scroll-table')[1].select('tbody tr')
      away_data = []
      for i, row in enumerate(away_table):
          stats = [stat.get_text(strip=True) for stat in row.find_all('td')]
          if i < len(away_player_names):  # 선수 이름과 매칭
              away_data.append([away_team_name, home_team_name, game_date, away_player_names[i]] + stats)

      # 홈팀 데이터프레임 생성
      home_df = pd.DataFrame(home_data, columns=columns)

      # 원정팀 데이터프레임 생성
      away_df = pd.DataFrame(away_data, columns=columns)

      # 홈팀과 원정팀 데이터를 하나로 합치기
      game_df = pd.concat([home_df, away_df], ignore_index=True)

      # 전체 데이터프레임에 추가
      all_data_df = pd.concat([all_data_df, game_df], ignore_index=True)
  all_data_df.to_csv(str(year)+'basketball_game_records.csv', index=False, encoding='utf-8-sig')


