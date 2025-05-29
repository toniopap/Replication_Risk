#%%
# generate a qrcode 
import qrcode
# %%
s = "https://docs.google.com/forms/d/e/1FAIpQLScancZvP0EQHHxQmxQrGq50QrlJZx9QDtbo_9A87H1lvIsPQw/viewform?usp=header"
# %%
img = qrcode.make(s)
type(img)
# %%
img.save("..\img\\formqrcode.png")
# %%
