"""UI smoke test for MAMC BioStat (Playwright). Start the app on port 8765 first."""
import sys, time
from playwright.sync_api import sync_playwright

URL = "http://127.0.0.1:8766"
OUT = sys.argv[1] if len(sys.argv) > 1 else "."
errors = []

def shot(page, name):
    page.wait_for_timeout(1200)
    page.screenshot(path=f"{OUT}/{name}.png", full_page=False)

def select(page, input_id, value):
    # selectize-backed select: set via Shiny
    page.evaluate(f"Shiny.setInputValue('{input_id}', {value!r})")

with sync_playwright() as p:
    b = p.chromium.launch()
    page = b.new_page(viewport={"width": 1440, "height": 950})
    page.on("console", lambda m: errors.append(m.text) if m.type == "error" else None)
    page.goto(URL); page.wait_for_selector("#demo_home"); shot(page, "01_home")
    page.click("#demo_messy"); page.wait_for_selector(".verdict-bad", timeout=15000); shot(page, "02_messy_check")
    page.click("#apply_fix"); page.wait_for_timeout(2500); shot(page, "03_after_fix")
    page.click("#demo"); page.wait_for_selector(".verdict-ok", timeout=15000); shot(page, "04_clean_check")
    # wizard
    page.click("a:has-text('2. Choose Test')"); page.wait_for_timeout(800)
    page.check("input[name=wz_aim][value=compare]"); page.check("input[name=wz_out][value=num]")
    page.check("input[name=wz_ng][value='3']"); page.check("input[name=wz_paired][value=ind]"); page.check("input[name=wz_norm][value=yes]")
    page.wait_for_selector("#wz_go"); shot(page, "05_wizard")
    page.click("#wz_go"); page.wait_for_timeout(2500)
    page.evaluate("$('#in_y')[0].selectize.setValue('SBP_week12')"); page.wait_for_timeout(500)
    page.click("#run"); page.wait_for_selector(".decision", timeout=20000); shot(page, "06_anova_results")
    page.click("a:has-text('Step-by-step calculation')"); page.wait_for_timeout(1500); shot(page, "07_anova_steps")
    page.click("a:has-text('Help: background')"); page.wait_for_timeout(1500); shot(page, "08_anova_help")
    page.click("a:has-text('Plot')"); page.wait_for_timeout(2500); shot(page, "09_anova_plot")
    # visualise
    page.click("a:has-text('4. Visualise')"); page.wait_for_timeout(3000); shot(page, "10_visualise")
    page.click("a:has-text('Python code for this chart')"); page.wait_for_timeout(1500); shot(page, "11_python")
    page.click("a:has-text('Report')"); page.wait_for_timeout(1500); shot(page, "12_report")
    page.click("a:has-text('Learn')"); page.wait_for_timeout(1500); shot(page, "13_learn")
    b.close()
print("console errors:", errors[:10])
