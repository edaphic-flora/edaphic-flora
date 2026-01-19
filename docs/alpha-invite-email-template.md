# Alpha Invite Email Template for Polished

Use this content when configuring your invite email in the Polished dashboard.

---

## Subject Line

You're invited to the edaphic flora alpha!

---

## Email Body (HTML)

```html
<div style="font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; max-width: 600px; margin: 0 auto; padding: 20px;">

  <div style="text-align: center; margin-bottom: 30px;">
    <h1 style="color: #373D3C; font-size: 28px; margin: 0;">
      <span style="color: #7A9A86;">edaphic</span> flora
    </h1>
    <p style="color: #666; font-size: 14px; margin-top: 5px;">Soil data for plant people</p>
  </div>

  <div style="background: #F7F4E8; border-radius: 8px; padding: 25px; margin-bottom: 25px;">
    <h2 style="color: #373D3C; margin-top: 0;">You're Invited!</h2>
    <p style="color: #444; line-height: 1.6;">
      Hi there! You've been selected to try <strong>edaphic flora</strong> during our private alpha phase.
    </p>
    <p style="color: #444; line-height: 1.6;">
      Edaphic flora is a community-driven database that connects soil conditions to plant success.
      By contributing your soil test data, you'll help build a resource that answers the question:
      <em>"What soil conditions help this plant thrive?"</em>
    </p>
  </div>

  <div style="margin-bottom: 25px;">
    <h3 style="color: #7A9A86; margin-bottom: 10px;">What You Can Do</h3>
    <ul style="color: #444; line-height: 1.8; padding-left: 20px;">
      <li><strong>Record soil data</strong> - Enter soil test results paired with plant species</li>
      <li><strong>Track outcomes</strong> - Note whether plants are thriving, struggling, or failed</li>
      <li><strong>Explore patterns</strong> - See pH, nutrient, and texture trends for any species</li>
      <li><strong>Upload reports</strong> - AI extracts data from soil test PDFs automatically</li>
    </ul>
  </div>

  <div style="margin-bottom: 25px;">
    <h3 style="color: #7A9A86; margin-bottom: 10px;">As an Alpha Tester</h3>
    <p style="color: #444; line-height: 1.6;">
      Your feedback is invaluable! Please let me know about any bugs, confusing interfaces,
      or features you'd like to see. You can report issues directly in the app or email me.
    </p>
    <p style="color: #666; font-size: 14px; font-style: italic;">
      Note: The database is starting fresh, so the more data you contribute, the more useful the analysis becomes!
    </p>
  </div>

  <div style="text-align: center; margin: 30px 0;">
    <a href="{{ app_url }}" style="background-color: #7A9A86; color: white; padding: 14px 28px; text-decoration: none; border-radius: 6px; font-weight: 600; display: inline-block;">
      Get Started
    </a>
  </div>

  <div style="border-top: 1px solid #ddd; padding-top: 20px; margin-top: 30px; text-align: center; color: #888; font-size: 13px;">
    <p style="margin: 0 0 10px 0;">
      Questions or feedback? Reply to this email or reach out at
      <a href="mailto:edaphicflora@gmail.com" style="color: #7A9A86;">edaphicflora@gmail.com</a>
    </p>
    <p style="margin: 0; color: #aaa;">
      Thank you for helping build something useful for the plant community!
    </p>
  </div>

</div>
```

---

## Plain Text Version

```
You're invited to the edaphic flora alpha!

Hi there! You've been selected to try edaphic flora during our private alpha phase.

Edaphic flora is a community-driven database that connects soil conditions to plant success. By contributing your soil test data, you'll help build a resource that answers the question: "What soil conditions help this plant thrive?"

WHAT YOU CAN DO:
- Record soil data - Enter soil test results paired with plant species
- Track outcomes - Note whether plants are thriving, struggling, or failed
- Explore patterns - See pH, nutrient, and texture trends for any species
- Upload reports - AI extracts data from soil test PDFs automatically

AS AN ALPHA TESTER:
Your feedback is invaluable! Please let me know about any bugs, confusing interfaces, or features you'd like to see. You can report issues directly in the app or email me.

Note: The database is starting fresh, so the more data you contribute, the more useful the analysis becomes!

GET STARTED: {{ app_url }}

Questions or feedback? Reply to this email or reach out at edaphicflora@gmail.com

Thank you for helping build something useful for the plant community!
```

---

## How to Configure in Polished

1. Log in to your Polished dashboard at https://dashboard.polished.tech
2. Navigate to your app settings
3. Find "Email Templates" or "Invite Email" section
4. Paste the HTML content above
5. Replace `{{ app_url }}` with your actual app URL if Polished doesn't auto-replace it
6. Send a test invite to yourself to preview

---

## Notes

- The `{{ app_url }}` placeholder should be automatically replaced by Polished with your app's sign-in URL
- If Polished doesn't support custom HTML, use the plain text version
- Brand colors used: Sage (#7A9A86), Charcoal (#373D3C), Limestone (#F7F4E8)
