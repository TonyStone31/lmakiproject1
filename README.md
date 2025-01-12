# Lazarus Mouse and KeyInput Package Improvements

This project continues the foundational work of **Tom Gregorovic**, enhancing the **Lazarus Mouse and KeyInput package** to support modern systems like **Wayland** and potentially others. The ultimate goal is to create a quality package that will allow you to build a complete **xdotool replacement** using **Free Pascal**.

---

## 🎯 Project Goals
- **Enhance Cross-Platform Support**:
  - Add and refine **Wayland** support, alongside existing support for **X Windows** and **Microsoft Windows**.
  - Continue improving and testing **macOS** compatibility (community contributions welcome!).
- **Provide a Free Pascal Alternative to xdotool**:
  - Enable precise mouse and keyboard automation across platforms.
  - Deliver a lightweight, open-source, and developer-friendly tool.

---

## 🛠 Current Status
### Platforms
- **Linux (X Windows)**:
  - Mouse and keyboard input functionality is stable and reliable.
- **Microsoft Windows**:
  - Most features have been successfully tested and are performing as expected.
- **Wayland**:
  - Basic functionality is operational, but:
    - **Requires elevated permissions** or proper user access to input devices.
    - Advanced features are still under exploration.
- **macOS**:
  - Currently untested due to lack of access to Mac systems.

---

## 🗓 Future Plans
- **Wayland**:
  - Resolve permission-related issues for easier usability.
  - Explore compositor-specific protocols for advanced functionality.
- **macOS**:
  - Enable macOS support through testing and contributions.
- **Additional Features**:
  - Fully replicate and enhance **xdotool** capabilities.
  - Ensure seamless operation across all supported platforms.

---

## 🤝 Contributions
Contributions are welcome! If you have experience with **Wayland**, **macOS**, or advanced input automation, your expertise is invaluable.

- Fork the repository and submit a pull request.
- Report issues or suggest features via [GitHub Issues](https://github.com/your-repo/issues).

---

## 📜 Acknowledgments
A thanks to **Tom Gregorovic** for intially creating this package. The initial work serves as the backbone of this project.

---

## 📚 Additional Resources
- [Free Pascal](https://www.freepascal.org/)
- [Lazarus IDE](https://www.lazarus-ide.org/)
- [xdotool](https://github.com/jordansissel/xdotool)
